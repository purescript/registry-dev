module Registry.App.CLI.Purs where

import Registry.App.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CA.Compat
import Data.Codec.Argonaut.Record as CA.Record
import Data.Foldable (foldMap)
import Data.String as String
import Node.Library.Execa as Execa
import Registry.Version as Version

-- | Call a specific version of the PureScript compiler
callCompiler_ :: { version :: Maybe Version, command :: PursCommand, cwd :: Maybe FilePath } -> Aff Unit
callCompiler_ = void <<< callCompiler

data CompilerFailure
  = CompilationError (Array CompilerError)
  | UnknownError String
  | MissingCompiler

derive instance Eq CompilerFailure
derive instance Ord CompilerFailure

compilerFailureCodec :: JsonCodec CompilerFailure
compilerFailureCodec = CA.codec' decode encode
  where
  decode :: Json -> Either JsonDecodeError CompilerFailure
  decode json =
    map CompilationError (CA.decode (CA.array compilerErrorCodec) json)
      <|> map UnknownError (CA.decode CA.string json)
      <|> map (const MissingCompiler) (CA.decode CA.null json)

  encode :: CompilerFailure -> Json
  encode = case _ of
    CompilationError errors -> CA.encode (CA.array compilerErrorCodec) errors
    UnknownError message -> CA.encode CA.string message
    MissingCompiler -> CA.encode CA.null unit

type CompilerError =
  { position :: SourcePosition
  , message :: String
  , errorCode :: String
  , errorLink :: String
  , filename :: FilePath
  , moduleName :: Maybe String
  }

compilerErrorCodec :: JsonCodec CompilerError
compilerErrorCodec = CA.Record.object "CompilerError"
  { position: sourcePositionCodec
  , message: CA.string
  , errorCode: CA.string
  , errorLink: CA.string
  , filename: CA.string
  , moduleName: CA.Compat.maybe CA.string
  }

type SourcePosition =
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }

sourcePositionCodec :: JsonCodec SourcePosition
sourcePositionCodec = CA.Record.object "SourcePosition"
  { startLine: CA.int
  , startColumn: CA.int
  , endLine: CA.int
  , endColumn: CA.int
  }

-- TODO: This would be better handled with dodo-printer.
printCompilerErrors :: Array CompilerError -> String
printCompilerErrors errors = do
  let
    total = Array.length errors
    printed =
      errors # Array.mapWithIndex \n error -> String.joinWith "\n"
        [ "Error " <> show (n + 1) <> " of " <> show total
        , ""
        , printCompilerError error
        , ""
        ]

  String.joinWith "\n" printed
  where
  printCompilerError :: CompilerError -> String
  printCompilerError { moduleName, filename, message, errorLink, position } =
    String.joinWith "\n"
      [ foldMap (\name -> "  Module: " <> name <> "\n") moduleName <> "  File: " <> filename <> "\n"
      , "  Message:"
      , ""
      , message
      -- The message has a newline, so no need for another.
      , "  Position:"
      , "  " <> show position.startLine <> ":" <> show position.startColumn <> " - " <> show position.endLine <> ":" <> show position.endColumn
      , ""
      , "  Error details:"
      , "  " <> errorLink
      ]

type CompilerArgs =
  { version :: Maybe Version
  , cwd :: Maybe FilePath
  , command :: PursCommand
  }

data PursCommand
  = Version
  | Compile { globs :: Array FilePath }
  | Publish { resolutions :: FilePath }
  | Graph { globs :: Array FilePath }

printCommand :: PursCommand -> Array String
printCommand = case _ of
  Version -> [ "--version" ]
  Compile { globs } -> [ "compile" ] <> globs <> [ "--json-errors" ]
  Publish { resolutions } -> [ "publish", "--manifest", "purs.json", "--resolutions", resolutions ]
  Graph { globs } -> [ "graph" ] <> globs <> [ "--json-errors" ]

-- | Call a specific version of the PureScript compiler
callCompiler :: CompilerArgs -> Aff (Either CompilerFailure String)
callCompiler compilerArgs = do
  let
    -- If no version is provided, uses 'purs'. Otherwise, converts a string version 'v0.13.0' or
    -- '0.13.0' to the standard format for executables, ie. 'purs-0_13_0'.
    purs =
      case compilerArgs.version of
        Nothing -> "purs"
        Just version ->
          append "purs-"
            $ String.replaceAll (String.Pattern ".") (String.Replacement "_")
            $ Version.print version

    errorsCodec = CA.Record.object "CompilerErrors"
      { errors: CA.array compilerErrorCodec
      }

  result <- _.result =<< Execa.execa purs (printCommand compilerArgs.command) (_ { cwd = compilerArgs.cwd })
  pure case result of
    Left { originalMessage }
      | originalMessage == Just (String.joinWith " " [ "spawn", purs, "ENOENT" ]) -> Left MissingCompiler
    Left { stdout, stderr } -> Left do
      let
        output = case compilerArgs.version of
          Nothing -> stdout
          Just version | Right min <- Version.parse "0.14.0", version < min -> stderr
          Just _ -> stdout
      case parseJson errorsCodec output of
        Left err -> UnknownError $ String.joinWith "\n" [ stdout, stderr, CA.printJsonDecodeError err ]
        Right ({ errors } :: { errors :: Array CompilerError })
          | Array.null errors -> UnknownError "Non-normal exit code, but no errors reported."
          | otherwise -> CompilationError errors
    Right { stdout } -> Right stdout
