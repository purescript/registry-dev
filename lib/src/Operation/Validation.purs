module Registry.Operation.Validation where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Data.Time.Duration (Hours(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path (FilePath)
import PureScript.CST as CST
import PureScript.CST.Errors as CST.Errors
import PureScript.CST.Types as CST.Types
import Registry.Location (Location)
import Registry.Manifest (Manifest(..))
import Registry.Metadata (Metadata(..), PublishedMetadata, UnpublishedMetadata)
import Registry.Operation (PublishData)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Solver (CompilerIndex)
import Registry.Solver as Solver
import Registry.Version (Version)

-- This module exports utilities for writing validation for `Registry Operations`.
-- See https://github.com/purescript/registry-dev/blob/master/SPEC.md#5-registry-operations

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
validateDependenciesSolve :: Version -> Manifest -> CompilerIndex -> Either Solver.SolverErrors (Map PackageName Version)
validateDependenciesSolve compiler (Manifest manifest) compilerIndex =
  map snd $ Solver.solveWithCompiler (Range.exact compiler) compilerIndex manifest.dependencies

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

-- | Validate that the given directory contains at least one PureScript module
-- | and all PureScript modules have well-formed module headers that the
-- | registry will accept (no forbidden names).
validatePursModules :: forall m. MonadAff m => NonEmptyArray FilePath -> m (Either String Unit)
validatePursModules files = do
  let
    acceptedPursModule :: FilePath -> m (Tuple FilePath (Either String Unit))
    acceptedPursModule path = liftAff do
      eitherModule <- Aff.attempt (FS.Aff.readTextFile UTF8 path)
      pure $ Tuple path $ case eitherModule of
        Left err -> Left $ "Could not read PureScript module from disk at path " <> path <> ": " <> Aff.message err
        Right moduleString -> validatePursModule moduleString

    convertErrors :: NonEmptyArray (Tuple FilePath (Either String Unit)) -> Array (Tuple FilePath String)
    convertErrors = NEA.toArray >>> Array.concatMap case _ of
      Tuple path (Left err) -> [ Tuple path err ]
      Tuple _ (Right _) -> []

  results <- traverse acceptedPursModule files

  case convertErrors results of
    [] -> pure $ Right unit
    converted -> pure $ Left $ Array.foldMap (\(Tuple path err) -> "\n  - " <> path <> ": " <> err) converted

-- | Module names that the registry has explicitly disallowed.
-- | https://github.com/purescript/registry-dev/issues/566
forbiddenModules :: Array String
forbiddenModules =
  [ "Main"
  , "Test.Main"
  ]

-- | Verify the given PureScript source file uses an accepted module name (some
-- | modules are reserved, such as 'Main' or 'Test.Main', because they are so
-- | common in user code).
validatePursModule :: String -> Either String Unit
validatePursModule moduleString = case CST.parsePartialModule moduleString of
  CST.ParseFailed err ->
    Left $ "Failed to parse PureScript module: " <> CST.Errors.printParseError err.error
  CST.ParseSucceededWithErrors (CST.PartialModule { header }) _ ->
    verifyHeader header
  CST.ParseSucceeded (CST.PartialModule { header }) ->
    verifyHeader header
  where
  verifyHeader :: forall e. CST.Types.ModuleHeader e -> Either String Unit
  verifyHeader (CST.Types.ModuleHeader { name: CST.Types.Name { name: CST.Types.ModuleName name } }) =
    if Array.notElem name forbiddenModules then
      Right unit
    else
      Left $ "Module name is " <> name <> " but PureScript libraries cannot publish modules named: " <> String.joinWith ", " forbiddenModules

-- | These files are always included in the tarball, if present
alwaysIncludedFilesIfPresent :: Set FilePath
alwaysIncludedFilesIfPresent = Set.fromFoldable
  [ "purs.json"
  , "spago.yaml"
  , "spago.dhall"
  , "packages.dhall"
  , "bower.json"
  , "package.json"
  ]

-- | Validate that the given directory contains no files that are excluded by
-- | the package's configuration. In case of error, return the set of files that
-- | shouldn't be excluded
validateNoExcludedObligatoryFiles :: Array FilePath -> Either (NonEmptySet FilePath) Unit
validateNoExcludedObligatoryFiles files = do
  let fileSet = Set.fromFoldable files
  let removed = Set.intersection alwaysIncludedFilesIfPresent fileSet
  let removedNonEmpty = NonEmptySet.fromSet removed
  maybe (Right unit) Left removedNonEmpty
