module Foreign.FastGlob
  ( GlobOptions(..)
  , Include(..)
  , SanitizedPaths
  , match
  , match'
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import ConvertableOptions (class Defaults)
import ConvertableOptions as ConvertableOptions
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Partial.Unsafe (unsafeCrashWith)

type SanitizedPaths = { succeeded :: Array FilePath, failed :: Array FilePath }

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
  sanitizePaths matches
  where
  options :: { | GlobOptions }
  options = ConvertableOptions.defaults defaultGlobOptions opts

  sanitizePaths :: Array FilePath -> Aff SanitizedPaths
  sanitizePaths paths = do
    sanitized <- traverse sanitizePath paths
    let { right, left } = separate sanitized
    pure { succeeded: right, failed: left }

  sanitizePath :: FilePath -> Aff (Either String FilePath)
  sanitizePath path = do
    absoluteRoot <- Aff.attempt (FS.realpath baseDirectory) >>= case _ of
      Left _ -> unsafeCrashWith $ "sanitizePath provided with a base directory that does not exist: " <> baseDirectory
      Right canonical -> pure canonical

    absolutePath <- Aff.attempt (FS.realpath $ Path.concat [ absoluteRoot, path ]) >>= case _ of
      Left _ -> unsafeCrashWith $ "sanitizePath provided with a path that does not exist: " <> path
      Right canonical -> pure canonical

    -- Protect against directory traversals
    pure $ case String.indexOf (String.Pattern absoluteRoot) absolutePath of
      Just 0 -> Right path
      _ -> Left path
