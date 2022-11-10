module Registry.Internal.Path
  ( sanitizePaths
  , SanitizedPaths
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Bifunctor (lmap)
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

type SanitizedPaths = { succeeded :: Array FilePath, failed :: Array FilePath }

sanitizePaths :: FilePath -> Array FilePath -> Aff SanitizedPaths
sanitizePaths baseDirectory paths = do
  sanitized <- traverse (sanitizePath baseDirectory) paths
  let { right, left } = separate sanitized
  pure { succeeded: right, failed: left }

sanitizePath :: FilePath -> FilePath -> Aff (Either String FilePath)
sanitizePath baseDirectory path = runExceptT do
  absoluteRoot <- ExceptT $ map (lmap (\_ -> "sanitizePath provided with a base directory that does not exist: " <> baseDirectory)) (Aff.attempt (FS.realpath baseDirectory))
  absolutePath <- ExceptT $ map (lmap (\_ -> "sanitizePath provided with a path that does not exist: " <> path)) (Aff.attempt (FS.realpath (Path.concat [ absoluteRoot, path ])))

  -- Protect against directory traversals
  ExceptT $ pure $ case String.indexOf (String.Pattern absoluteRoot) absolutePath of
    Just 0 -> Right path
    _ -> Left path
