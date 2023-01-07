module Registry.Internal.Path where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Path as Path

-- | Given a directory, validate a file path relative to that root to ensure
-- | that file path does not traverse out of the directory.
-- | Paths are expanded using `FS.realpath` to follow symlinks.
sanitizePath :: forall m. MonadAff m => FilePath -> FilePath -> m (Either String FilePath)
sanitizePath baseDirectory path = liftAff $ runExceptT do
  absoluteRoot <- ExceptT $ map (lmap (\_ -> "sanitizePath provided with a base directory that does not exist: " <> baseDirectory)) (Aff.attempt (FS.realpath baseDirectory))
  absolutePath <- ExceptT $ map (lmap (\_ -> "sanitizePath provided with a path that does not exist: " <> path)) (Aff.attempt (FS.realpath (Path.concat [ absoluteRoot, path ])))

  -- Protect against directory traversals
  ExceptT $ pure $ case String.indexOf (String.Pattern absoluteRoot) absolutePath of
    Just 0 -> Right path
    _ -> Left path
