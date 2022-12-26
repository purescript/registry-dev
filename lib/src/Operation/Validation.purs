module Registry.Operation.Validation where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration (Hours(..))
import Data.Traversable (for, traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Node.FS.Aff as FS.Aff
import Node.FS.Stats as Stats
import Node.Path (FilePath)
import Node.Path as Path
import Registry.Location (Location)
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex (ManifestIndex)
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata (Metadata(..), PublishedMetadata, UnpublishedMetadata)
import Registry.Operation (PublishData)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Solver as Solver
import Registry.Version (Version)

-- This module exports utilities for writing validation for `Registry Operations`.
-- See https://github.com/purescript/registry-dev/blob/master/SPEC.md#5-registry-operations

-- | Checks that there is at least one purs file within the `src` directory.
containsPursFile :: FilePath -> Aff Boolean
containsPursFile parent = flip catchError (\_ -> pure false) do
  children <- FS.Aff.readdir parent

  stats <- for children \relpath -> do
    let path = Path.concat [ parent, relpath ]
    stats <- FS.Aff.stat path
    pure { path, isDirectory: Stats.isDirectory stats }

  let { no: files, yes: directories } = Array.partition _.isDirectory stats

  if Array.any (\{ path } -> isJust (String.stripSuffix (Pattern ".purs") path)) files then
    pure true
  else do
    results <- traverse (_.path >>> containsPursFile) directories
    pure $ Array.any (eq true) results

-- | Checks that the manifest package name and the PublishData payload package name match.
nameMatches :: Manifest -> PublishData -> Boolean
nameMatches (Manifest manifestFields) { name } =
  manifestFields.name == name

-- | Checks that the manifest location and metadata location match.
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

-- | Verifies that the manifest dependencies are solvable by the registry solver.
validateDependenciesSolve :: Manifest -> ManifestIndex -> Either (NonEmptyList Solver.SolverError) (Map PackageName Version)
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

data TarballSizeResult
  = ExceedsMaximum Number
  | WarnPackageSize Number

-- | Verifies that the Tarball size is under the registry maximum, and warns if it is close to that maximum.
validateTarballSize :: Number -> Maybe TarballSizeResult
validateTarballSize size =
  if size > maxPackageBytes then
    Just (ExceedsMaximum maxPackageBytes)
  else if size > warnPackageBytes then
    Just (WarnPackageSize warnPackageBytes)
  else
    Nothing
  where
  -- | The absolute maximum bytes allowed in a package
  maxPackageBytes :: Number
  maxPackageBytes = 2_000_000.0

  -- | The number of bytes over which we flag a package for review
  warnPackageBytes :: Number
  warnPackageBytes = 200_000.0

locationIsUnique :: Location -> Map PackageName Metadata -> Boolean
locationIsUnique location = Map.isEmpty <<< Map.filter (eq location <<< _.location <<< un Metadata)

data UnpublishError
  = NotPublished
  | AlreadyUnpublished
  | InternalError
  | PastTimeLimit { limit :: Hours, difference :: Hours }

derive instance Eq UnpublishError

-- | Verifies that a package version is eligible for unpublishing.
-- | The version must have been published before, must not have been unpublished before, and unpublishing
-- | must happen within the 48 hours following publishing.
validateUnpublish :: DateTime -> Version -> Metadata -> Either UnpublishError PublishedMetadata
validateUnpublish now version (Metadata metadata) = do
  let
    inPublished = Map.lookup version metadata.published
    inUnpublished = Map.lookup version metadata.unpublished

  case inPublished, inUnpublished of
    Nothing, Nothing ->
      Left NotPublished
    Just published, Nothing -> do
      -- We only pass through the case where the user is unpublishing a
      -- package that has been published and not yet unpublished.
      let diff = DateTime.diff now published.publishedTime
      if (diff > hourLimit) then
        Left $ PastTimeLimit { limit: hourLimit, difference: diff }
      else
        Right published
    Nothing, Just _ ->
      Left AlreadyUnpublished
    Just _, Just _ ->
      Left InternalError
  where
  hourLimit = Hours 48.0
