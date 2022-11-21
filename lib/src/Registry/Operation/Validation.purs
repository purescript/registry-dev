module Registry.Operation.Validation where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Aff (Aff)
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as Stats
import Node.Path (FilePath)
import Registry.Metadata (Metadata(..), PublishedMetadata, UnpublishedMetadata)
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex (ManifestIndex)
import Registry.ManifestIndex as ManifestIndex
import Registry.Operation (PublishData)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Solver as Solver
import Registry.Version (Version)

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

isMetadataPackage :: Manifest -> Boolean
isMetadataPackage (Manifest { name }) = PackageName.print name == "metadata"

-- | Checks that the Manifest version has not been published before, according to the metadata file.
-- | If the Manifest version has been published before, returns previous info.
isNotPublished :: Manifest -> Metadata -> Maybe PublishedMetadata
isNotPublished (Manifest { version }) (Metadata { published }) =
  Map.lookup version published

-- | Checks that the Manifest version has not been unpublished before, according to the metadata file.
-- | If the Manifest version has been unpublished before, returns previous info.
isNotUnpublished :: Manifest -> Metadata -> Maybe UnpublishedMetadata
isNotUnpublished (Manifest { version }) (Metadata { unpublished }) =
  Map.lookup version unpublished

validateDependenciesSolve :: Manifest -> ManifestIndex -> Either (NonEmptyArray Solver.SolverError) (Map PackageName Version)
validateDependenciesSolve manifest manifestIndex = do
  let getDependencies = _.dependencies <<< un Manifest
  Solver.solve (map (map getDependencies) (ManifestIndex.toMap manifestIndex)) (getDependencies manifest)

-- | Verifies that all dependencies in the manifest are present in the build
-- | plan, and the version listed in the build plan is within the range provided
-- | in the manifest. Note: this only checks dependencies listed in the manifest
-- | and will ignore transitive dependecies.
getUnresolvedDependencies :: Manifest -> Map PackageName Version -> Array (Either (PackageName /\ Range) (PackageName /\ Range /\ Version))
getUnresolvedDependencies (Manifest { dependencies }) resolutions =
  Array.mapMaybe (uncurry dependencyUnresolved) (Map.toUnfoldable dependencies)
  where
  dependencyUnresolved :: PackageName -> Range -> Maybe (Either (PackageName /\ Range) (PackageName /\ Range /\ Version))
  dependencyUnresolved dependencyName dependencyRange =
    case Map.lookup dependencyName resolutions of
      -- If the package is missing from the build plan then the plan is incorrect.
      Nothing -> Just $ Left $ dependencyName /\ dependencyRange
      -- If the package exists, but the version is not in the manifest range
      -- then the build plan is incorrect. Otherwise, this part of the build
      -- plan is correct.
      Just version
        | not (Range.includes dependencyRange version) -> Just $ Right $ dependencyName /\ dependencyRange /\ version
        | otherwise -> Nothing


