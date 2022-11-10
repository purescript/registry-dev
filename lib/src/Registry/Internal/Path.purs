module Registry.Internal.Path
  ( sanitizePaths
  , SanitizedPaths
  ) where

import Prelude

import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path
import Partial.Unsafe (unsafeCrashWith)

type SanitizedPaths = { succeeded :: Array FilePath, failed :: Array FilePath }

sanitizePaths :: FilePath -> Array FilePath -> Aff SanitizedPaths
sanitizePaths baseDirectory paths = do
  sanitized <- traverse (sanitizePath baseDirectory) paths
  let { right, left } = separate sanitized
  pure { succeeded: right, failed: left }

sanitizePath :: FilePath -> FilePath -> Aff (Either String FilePath)
sanitizePath baseDirectory path = do
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
