module Registry.Operation.Validation where

import Prelude

import Data.Array as Array
import Data.Maybe (isJust)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as Stats
import Node.Path (FilePath)
import Registry.Metadata (Metadata(..))
import Registry.Manifest (Manifest(..))
import Registry.Operation (PublishData)

-- This module exports utilities for writing validation for `Registry Operations`.
-- See https://github.com/purescript/registry-dev/blob/master/SPEC.md#5-registry-operations

-- publish ::
-- transfer ::

-- | Checks that there is at least one purs file within the `src` directory.
-- TODO: What is `src` directory doesn't exist?
containsPursFile :: FilePath -> Aff Boolean
containsPursFile parent = do
  children <- FS.Aff.readdir parent
  stats <- traverse (\path -> { path, stats: _ } <$> FS.Aff.stat path) children
  let 
    files = Array.filter (_.stats >>> Stats.isFile) stats
    directories = Array.filter (_.stats >>> Stats.isDirectory) stats

  if Array.any (\{ path } -> isJust (String.stripSuffix (Pattern ".purs") path)) files then
    pure true
  else do
    results <- traverse (_.path >>> containsPursFile) directories 
    pure $ Array.any (eq true) results

-- | Checks that the manifest package name and the PublishData payload package name match.
nameMatches :: Manifest -> PublishData -> Boolean
nameMatches (Manifest manifestFields) { name } =
  manifestFields.name == name

-- | Checks that the manifest location and JSON payload location match.
locationMatches :: Manifest -> Metadata -> Boolean
locationMatches (Manifest manifestFields) (Metadata metadataFields) =
  manifestFields.location == metadataFields.location
