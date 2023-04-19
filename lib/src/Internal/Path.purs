module Registry.Internal.Path where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.String.Regex.Unsafe as Regex.Unsafe
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Node.FS.Aff as FS
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as FS.Stats
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

-- | Read all .purs files in the given directory.
readPursFiles :: forall m. MonadAff m => FilePath -> m (Maybe (NonEmptyArray FilePath))
readPursFiles init = liftAff do
  result <- Aff.attempt (go 0 init)
  case result of
    Left _ -> pure Nothing
    Right files -> pure (NEA.fromArray files)
  where
  go :: Int -> FilePath -> Aff (Array FilePath)
  go depth root = do
    FS.Aff.readdir root >>= Array.foldMap \file -> do
      let path = Path.concat [ root, Path.sep, file ]
      stats <- FS.Aff.stat path
      if FS.Stats.isDirectory stats then
        go (depth + 1) path
      else if Regex.test pursFileExtensionRegex path then
        pure [ path ]
      else pure []

pursFileExtensionRegex :: Regex
pursFileExtensionRegex = Regex.Unsafe.unsafeRegex "\\.purs$" Regex.Flags.noFlags
