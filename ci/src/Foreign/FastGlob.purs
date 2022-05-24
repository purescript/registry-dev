module Foreign.FastGlob
  ( Glob
  , GlobError(..)
  , GlobErrorReason(..)
  , GlobOptions(..)
  , Include(..)
  , defaultGlobOptions
  , match
  , parseGlob
  , parseGlobs
  , printGlob
  , printGlobError
  , unsafeMatch
  , unsafeMatch'
  ) where

import Registry.Prelude

import Control.Monad.Except (runExceptT)
import Control.Promise (Promise)
import Control.Promise as Promise
import ConvertableOptions (class Defaults)
import ConvertableOptions as ConvertableOptions
import Data.Array as Array
import Data.Compactable (separate)
import Data.String as String
import Node.Path as Path

-- | A glob pattern that has been sanitized so it does not allow null byte or
-- | directory traversal attacks.
--
-- Do not add a newtype instance.
newtype Glob = Glob String

derive instance Eq Glob

type GlobError = { pattern :: String, reason :: GlobErrorReason }

data GlobErrorReason = NullByte | DirectoryTraversal

derive instance Eq GlobErrorReason

instance Show GlobErrorReason where
  show = case _ of
    NullByte -> "NullByte"
    DirectoryTraversal -> "DirectoryTraversal"

printGlobError :: GlobError -> String
printGlobError { reason, pattern } =
  Array.fold
    [ "Parsing glob '"
    , pattern
    , "' failed: "
    , case reason of
        NullByte -> "glob patterns cannot contain null bytes."
        DirectoryTraversal -> "glob patterns cannot match beyond the base directory."
    ]

parseGlobs :: FilePath -> Array String -> Aff { failed :: Array GlobError, succeeded :: Array Glob }
parseGlobs baseDirectory globs = do
  parsed <- traverse (parseGlob baseDirectory) globs
  let { right, left } = separate parsed
  pure { succeeded: right, failed: left }

parseGlob :: FilePath -> String -> Aff (Either GlobError Glob)
parseGlob baseDirectory glob = runExceptT do
  case String.indexOf (String.Pattern glob) (String.singleton $ String.codePointFromChar '\x0') of
    Just _ -> throwError { reason: NullByte, pattern: glob }
    _ -> pure unit

  -- The `resolve` function normalizes and joins paths, so `..` and `.` are
  -- removed and result is an absolute path. We can then verify that the
  -- resulting path does not start above the given base directory.
  absoluteRoot <- liftEffect $ Path.resolve [] baseDirectory
  absoluteGlob <- liftEffect $ Path.resolve [ absoluteRoot ] glob

  case String.indexOf (String.Pattern absoluteRoot) absoluteGlob of
    Just 0 -> pure $ Glob glob
    _ -> throwError { reason: DirectoryTraversal, pattern: glob }

printGlob :: Glob -> String
printGlob (Glob pattern) = pattern

data Include = FilesAndDirectories | FilesOnly | DirectoriesOnly

derive instance Eq Include

-- https://github.com/mrmlnc/fast-glob#options-3
type GlobOptions =
  ( cwd :: Maybe FilePath
  , ignore :: Array FilePath
  , absolute :: Boolean
  , include :: Include
  , caseSensitive :: Boolean
  , dotfiles :: Boolean
  , unique :: Boolean
  )

defaultGlobOptions :: { | GlobOptions }
defaultGlobOptions =
  { cwd: Nothing
  , ignore: []
  , absolute: false
  , include: FilesAndDirectories
  , caseSensitive: true
  , dotfiles: true
  , unique: true
  }

type JSGlobOptions =
  { cwd :: Nullable FilePath
  , ignore :: Array String
  , absolute :: Boolean
  , onlyDirectories :: Boolean
  , onlyFiles :: Boolean
  , caseSensitive :: Boolean
  , dotfiles :: Boolean
  , unique :: Boolean
  }

globOptionsToJSGlobOptions :: { | GlobOptions } -> JSGlobOptions
globOptionsToJSGlobOptions options =
  { cwd: toNullable options.cwd
  , ignore: options.ignore
  , absolute: options.absolute
  , onlyDirectories: options.include == DirectoriesOnly
  , onlyFiles: options.include == FilesOnly
  , caseSensitive: options.caseSensitive
  , dotfiles: options.dotfiles
  , unique: options.unique
  }

foreign import matchImpl :: Array String -> JSGlobOptions -> Effect (Promise (Array FilePath))

-- | Match the provided list of glob patterns.
match :: Array Glob -> Aff (Array FilePath)
match = unsafeMatch <<< map (\(Glob pattern) -> pattern)

-- | Match the provided list of glob patterns. This is unsafe because it is
-- | vulnerable to null byte and directory traversal attacks. Use `safeMatch`
-- | instead if possible.
unsafeMatch :: Array String -> Aff (Array FilePath)
unsafeMatch entries = unsafeMatch' entries {}

-- | Match the provided list of glob patterns using the given glob options. This
-- | is unsafe because it is vulnerable to null byte and directory traversal
-- | attacks. Use `safeMatch'` instead if possible.
unsafeMatch'
  :: forall provided
   . Defaults { | GlobOptions } { | provided } { | GlobOptions }
  => Array String
  -> { | provided }
  -> Aff (Array FilePath)
unsafeMatch' entries provided = Promise.toAffE $ matchImpl entries $ globOptionsToJSGlobOptions options
  where
  options :: { | GlobOptions }
  options = ConvertableOptions.defaults defaultGlobOptions provided
