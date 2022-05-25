module Foreign.FastGlob
  ( GlobOptions(..)
  , Include(..)
  , SafePathError
  , SafePathErrorReason(..)
  , SanitizedPaths
  , match
  , match'
  , printSafePathError
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
import Effect.Aff as Aff
import Node.FS.Aff as FS
import Node.Path as Path

type SafePathError = { path :: FilePath, reason :: SafePathErrorReason }

data SafePathErrorReason
  = DirectoryTraversal
  | BaseDirectoryMissing
  | PathMissing

derive instance Eq SafePathErrorReason

instance Show SafePathErrorReason where
  show = case _ of
    DirectoryTraversal -> "DirectoryTraversal"
    BaseDirectoryMissing -> "BaseDirectoryMissing"
    PathMissing -> "PathMissing"

printSafePathError :: SafePathError -> String
printSafePathError { reason, path } =
  Array.fold
    [ "Sanitizing '"
    , path
    , "' failed: "
    , case reason of
        DirectoryTraversal -> "file paths cannot be outside the base directory."
        BaseDirectoryMissing -> "the base directory does not exist."
        PathMissing -> "the provided path does not exist."
    ]

type SanitizedPaths = { succeeded :: Array FilePath, failed :: Array SafePathError }

sanitizePaths :: FilePath -> Array FilePath -> Aff SanitizedPaths
sanitizePaths baseDirectory paths = do
  sanitized <- traverse (sanitizePath <<< { baseDirectory, path: _ }) paths
  let { right, left } = separate sanitized
  pure { succeeded: right, failed: left }

sanitizePath :: { baseDirectory :: FilePath, path :: FilePath } -> Aff (Either SafePathError FilePath)
sanitizePath { baseDirectory, path } = runExceptT do
  absoluteRoot <- liftAff (Aff.attempt $ FS.realpath baseDirectory) >>= case _ of
    Left _ -> throwError { reason: BaseDirectoryMissing, path: baseDirectory }
    Right canonical -> pure canonical

  absolutePath <- liftAff (Aff.attempt $ FS.realpath $ Path.concat [ absoluteRoot, path ]) >>= case _ of
    Left _ -> throwError { reason: PathMissing, path }
    Right canonical -> pure canonical

  case String.indexOf (String.Pattern absoluteRoot) absolutePath of
    Just 0 -> pure path
    _ -> throwError { reason: DirectoryTraversal, path }

data Include = FilesAndDirectories | FilesOnly | DirectoriesOnly

derive instance Eq Include

-- https://github.com/mrmlnc/fast-glob#options-3
type GlobOptions =
  ( ignore :: Array FilePath
  , include :: Include
  , caseSensitive :: Boolean
  , dotfiles :: Boolean
  , unique :: Boolean
  )

defaultGlobOptions :: { | GlobOptions }
defaultGlobOptions =
  { ignore: []
  , include: FilesAndDirectories
  , caseSensitive: true
  , dotfiles: true
  , unique: true
  }

type JSGlobOptions =
  { cwd :: String
  , ignore :: Array String
  , onlyDirectories :: Boolean
  , onlyFiles :: Boolean
  , caseSensitive :: Boolean
  , dotfiles :: Boolean
  , unique :: Boolean
  }

globOptionsToJSGlobOptions :: FilePath -> { | GlobOptions } -> JSGlobOptions
globOptionsToJSGlobOptions cwd options = do
  { cwd
  , ignore: options.ignore
  , onlyDirectories: options.include == DirectoriesOnly
  , onlyFiles: options.include == FilesOnly
  , caseSensitive: options.caseSensitive
  , dotfiles: options.dotfiles
  , unique: options.unique
  }

foreign import matchImpl :: Array String -> JSGlobOptions -> Effect (Promise (Array FilePath))

-- | Match the provided list of glob patterns.
match :: FilePath -> Array String -> Aff SanitizedPaths
match baseDir entries = match' baseDir entries {}

-- | Match the provided list of glob patterns using the given glob options.
match'
  :: forall provided
   . Defaults { | GlobOptions } { | provided } { | GlobOptions }
  => FilePath
  -> Array String
  -> { | provided }
  -> Aff SanitizedPaths
match' baseDirectory entries opts = do
  let jsOptions = globOptionsToJSGlobOptions baseDirectory options
  matches <- Promise.toAffE $ matchImpl entries jsOptions
  sanitizePaths baseDirectory matches
  where
  options = ConvertableOptions.defaults defaultGlobOptions opts
