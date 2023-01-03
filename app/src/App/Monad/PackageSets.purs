-- | An effect for upgrading package sets, either in batch mode (all suggested
-- | changes must go in together) or in sequential mode (as many changes as
-- | possible will be added, and changes that would break the set are recorded.)
module Registry.App.Monad.PackageSets where

import Registry.App.Prelude

import Control.Alternative (guard)
import Control.Monad.Except as Except
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array as Array
import Data.DateTime as DateTime
import Data.Filterable as Filterable
import Data.Foldable (foldl)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map as Map
import Data.Monoid as Monoid
import Data.Set as Set
import Data.String as String
import Data.Tuple (uncurry)
import Effect.Ref as Ref
import Node.FS.Aff as FS.Aff
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Registry.App.CLI.Purs (CompilerFailure(..))
import Registry.App.CLI.Purs as Purs
import Registry.App.CLI.Tar as Tar
import Registry.App.Monad.Log (class MonadLog)
import Registry.App.Monad.Log as Log
import Registry.App.Monad.Registry (class MonadRegistry)
import Registry.App.Monad.Registry as Registry
import Registry.App.Monad.Storage (class MonadStorage)
import Registry.App.Monad.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Version as Version

data Change = Update Version | Remove

derive instance Eq Change

type ChangeSet = Map PackageName Change

type SequentialUpgradeResult =
  { failed :: ChangeSet
  , succeeded :: ChangeSet
  , result :: PackageSet
  }

class Monad m <= MonadPackageSets m where
  -- | Upgrade the given package set using the provided compiler version and set
  -- | of changes. If any change fails, then the upgrade is aborted and the
  -- | unsuccessful changes are returned.
  upgradeAtomic :: PackageSet -> Version -> ChangeSet -> m (Either String (Maybe PackageSet))
  -- | Upgrade the given package set using the provided compiler version and set
  -- | of changes. Any successful change is applied, and any unsuccessful changes
  -- | are returned along with the new package set.
  upgradeSequential :: PackageSet -> Version -> ChangeSet -> m (Either String (Maybe SequentialUpgradeResult))

instance MonadPackageSets m => MonadPackageSets (ExceptT e m) where
  upgradeAtomic set version = lift <<< upgradeAtomic set version
  upgradeSequential set version = lift <<< upgradeSequential set version

type PackageSetsEnv =
  { workdir :: FilePath
  }

type PACKAGE_SETS r = (packageSets :: PackageSetsEnv | r)

handleUpgradeAtomic
  :: forall m r
   . MonadAsk { | PACKAGE_SETS + r } m
  => MonadRegistry m
  => MonadStorage m
  => MonadLog m
  => MonadAff m
  => PackageSet
  -> Version
  -> ChangeSet
  -> m (Either String (Maybe PackageSet))
handleUpgradeAtomic oldSet@(PackageSet { packages }) compiler changes = Except.runExceptT do
  env <- asks _.packageSets
  Log.info $ "Performing atomic upgrade of package set " <> Version.print (un PackageSet oldSet).version

  -- It is possible to reuse a workdir when processing package set batches, so
  -- we need to clean up before doing work.
  for_ [ packagesWorkDir env, outputWorkDir env, backupWorkDir env ] \dir -> do
    exists <- liftEffect $ FS.Sync.exists dir
    when exists do
      Log.debug $ "Removing existing working directory " <> dir
      FS.Extra.remove dir

  installPackages packages
  compileInstalledPackages compiler >>= case _ of
    Left compilerError -> do
      logCompilerError compiler compilerError
      Except.throwError "Compilation failed, but the starting package set must compile in order to process a batch."
    Right _ -> pure unit

  attemptChanges compiler oldSet changes >>= case _ of
    Left error -> do
      logCompilerError compiler error
      pure Nothing
    Right pending -> do
      newSet <- updatePackageSetMetadata compiler { previous: oldSet, pending } changes
      validatePackageSet newSet
      pure (Just newSet)

handleUpgradeSequential
  :: forall m r
   . MonadAsk { | PACKAGE_SETS + r } m
  => MonadRegistry m
  => MonadStorage m
  => MonadLog m
  => MonadAff m
  => PackageSet
  -> Version
  -> ChangeSet
  -> m (Either String (Maybe SequentialUpgradeResult))
handleUpgradeSequential oldSet@(PackageSet { packages }) compiler changes = Except.runExceptT do
  atomic <- Except.ExceptT $ handleUpgradeAtomic oldSet compiler changes
  case atomic of
    Just result -> pure $ Just { failed: Map.empty, succeeded: changes, result }
    Nothing -> do
      Log.info $ "Performing sequential upgrade of package set " <> Version.print (un PackageSet oldSet).version
      index <- Except.ExceptT Registry.readAllManifests

      let
        sortedPackages = ManifestIndex.toSortedArray index
        sortedBatch = sortedPackages # Array.mapMaybe \(Manifest { name, version }) -> do
          update <- Map.lookup name changes
          case update of
            Remove -> do
              prevVersion <- Map.lookup name packages
              guard (version == prevVersion)
              pure (Tuple name Remove)
            Update updateVersion -> do
              guard (version == updateVersion)
              pure (Tuple name (Update version))

      failRef <- liftEffect $ Ref.new Map.empty
      successRef <- liftEffect $ Ref.new Map.empty
      packageSetRef <- liftEffect $ Ref.new oldSet

      for_ sortedBatch \(Tuple name change) -> do
        currentSet <- liftEffect $ Ref.read packageSetRef
        attemptPackage compiler currentSet name change >>= case _ of
          -- If the package could not be processed, then the state of the
          -- filesystem is rolled back by attemptPackage. We just need to insert
          -- the package into the failures map.
          Left error -> do
            liftEffect $ Ref.modify_ (Map.insert name change) failRef
            Log.warn $ case change of
              Remove -> "Could not remove " <> PackageName.print name
              Update version -> "Could not add or update " <> formatPackageVersion name version
            logCompilerError compiler error

          -- If the package could be processed, then the state of the filesystem
          -- is stepped and we need to record the success of this package and
          -- step the package set in memory for the next package.
          Right newSet -> do
            Log.debug "Writing successful result and new package set to refs."
            liftEffect do
              Ref.modify_ (Map.insert name change) successRef
              Ref.write newSet packageSetRef
            Log.info $ case change of
              Remove -> "Removed " <> PackageName.print name
              Update version -> "Added or updated " <> formatPackageVersion name version

      failed <- liftEffect $ Ref.read failRef
      succeeded <- liftEffect $ Ref.read successRef
      pending <- liftEffect $ Ref.read packageSetRef

      case Map.size succeeded of
        0 -> pure Nothing
        _ -> do
          newSet <- updatePackageSetMetadata compiler { previous: oldSet, pending } changes
          validatePackageSet newSet
          pure $ Just { failed, succeeded, result: newSet }

packagesWorkDir :: PackageSetsEnv -> FilePath
packagesWorkDir { workdir } = Path.concat [ workdir, "packages" ]

outputWorkDir :: PackageSetsEnv -> FilePath
outputWorkDir { workdir } = Path.concat [ workdir, "output" ]

backupWorkDir :: PackageSetsEnv -> FilePath
backupWorkDir { workdir } = Path.concat [ workdir, "output-backup" ]

-- Install all packages in a package set into a temporary directory, returning
-- the reference to the installation directory. Installed packages have the
-- form: "package-name-major.minor.patch" and are stored in the "packages"
-- directory.
installPackages
  :: forall m r
   . MonadAsk { | PACKAGE_SETS + r } m
  => MonadStorage m
  => MonadLog m
  => MonadAff m
  => Map PackageName Version
  -> ExceptT String m Unit
installPackages packages = do
  Log.debug "Installing package set packages..."
  env <- asks _.packageSets
  FS.Extra.ensureDirectory (packagesWorkDir env)
  forWithIndex_ packages installPackage

-- Install a package into the packages directory, skipping installation if a
-- previous installation already exists.
installPackage
  :: forall m r
   . MonadAsk { | PACKAGE_SETS + r } m
  => MonadStorage m
  => MonadLog m
  => MonadAff m
  => PackageName
  -> Version
  -> ExceptT String m Unit
installPackage name version = do
  let formattedName = formatPackageVersion name version
  let extractedName = PackageName.print name <> "-" <> Version.print version
  let tarballName = extractedName <> ".tar.gz"
  env <- asks _.packageSets
  let dir = packagesWorkDir env
  let tarballPath = Path.concat [ dir, tarballName ]
  let extractedPath = Path.concat [ dir, extractedName ]
  let installPath = Path.concat [ dir, formattedName ]
  unlessM (liftEffect (FS.Sync.exists installPath)) $ do
    Log.debug $ "Installing " <> formattedName
    Except.ExceptT $ Storage.download name version tarballPath
    Tar.extract { cwd: packagesWorkDir env, archive: tarballName }
    FS.Extra.remove tarballPath
    liftAff $ FS.Aff.rename extractedPath installPath

-- Delete packages from the packages directory
removePackages
  :: forall m r
   . MonadAsk { | PACKAGE_SETS + r } m
  => MonadLog m
  => MonadAff m
  => Map PackageName Version
  -> m Unit
removePackages = traverseWithIndex_ removePackage

-- | Delete a package from the packages directory
removePackage
  :: forall m r
   . MonadAsk { | PACKAGE_SETS + r } m
  => MonadLog m
  => MonadAff m
  => PackageName
  -> Version
  -> m Unit
removePackage name version = do
  let formatted = formatPackageVersion name version
  Log.debug $ "Uninstalling " <> formatted
  env <- asks _.packageSets
  FS.Extra.remove $ Path.concat [ packagesWorkDir env, formatted ]

-- | Compile all PureScript files in the given directory. Expects all packages
-- | to be installed in a subdirectory "packages".
compileInstalledPackages
  :: forall m r
   . MonadAsk { | PACKAGE_SETS + r } m
  => MonadLog m
  => MonadAff m
  => Version
  -> m (Either CompilerFailure String)
compileInstalledPackages compiler = do
  Log.debug "Compiling installed packages..."
  env <- asks _.packageSets
  let command = Purs.Compile { globs: [ Path.concat [ Path.basename (packagesWorkDir env), "**/*.purs" ] ] }
  let version = Version.print compiler
  liftAff $ Purs.callCompiler { command, version, cwd: Just env.workdir }

attemptChanges
  :: forall m r
   . MonadAsk { | PACKAGE_SETS + r } m
  => MonadStorage m
  => MonadLog m
  => MonadAff m
  => Version
  -> PackageSet
  -> ChangeSet
  -> ExceptT String m (Either CompilerFailure PackageSet)
attemptChanges compiler (PackageSet set) changes = do
  env <- asks _.packageSets
  Log.debug "Attempting batch of changes..."
  FS.Extra.copy { from: outputWorkDir env, to: backupWorkDir env, preserveTimestamps: true }
  let
    additions = changes # Map.mapMaybe case _ of
      Remove -> Nothing
      Update version -> Just version
    oldVersions = Map.fromFoldable do
      Tuple name _ <- Map.toUnfoldable changes
      case Map.lookup name set.packages of
        Nothing -> []
        Just version -> [ Tuple name version ]
  removePackages oldVersions
  installPackages additions
  compileInstalledPackages compiler >>= case _ of
    Left err -> do
      FS.Extra.remove (outputWorkDir env)
      FS.Extra.copy { from: backupWorkDir env, to: outputWorkDir env, preserveTimestamps: true }
      removePackages additions
      installPackages oldVersions
      pure $ Left err
    Right _ -> do
      FS.Extra.remove (backupWorkDir env)
      let
        foldFn name existingSet = case _ of
          Remove -> Map.delete name existingSet
          Update version -> Map.insert name version existingSet
        newSet = foldlWithIndex foldFn set.packages changes
      pure $ Right $ PackageSet $ set { packages = newSet }

-- Attempt to add, update, or remove a package in the package set. This
-- operation will be rolled back if the addition fails.
--
-- NOTE: You must have previously built a package set.
attemptPackage
  :: forall m r
   . MonadAsk { | PACKAGE_SETS + r } m
  => MonadStorage m
  => MonadLog m
  => MonadAff m
  => Version
  -> PackageSet
  -> PackageName
  -> Change
  -> ExceptT String m (Either CompilerFailure PackageSet)
attemptPackage compiler (PackageSet set) package change = do
  env <- asks _.packageSets
  FS.Extra.copy { from: outputWorkDir env, to: backupWorkDir env, preserveTimestamps: true }
  let maybeOldVersion = Map.lookup package set.packages
  for_ maybeOldVersion (removePackage package)
  case change of
    Remove -> pure unit
    Update version -> installPackage package version
  compileInstalledPackages compiler >>= case _ of
    Left err -> do
      Log.info $ case change of
        Remove -> "Failed to build set without " <> PackageName.print package
        Update version -> "Failed to build set with " <> formatPackageVersion package version
      FS.Extra.remove (outputWorkDir env)
      FS.Extra.copy { from: backupWorkDir env, to: outputWorkDir env, preserveTimestamps: true }
      case change of
        Remove -> pure unit
        Update version -> removePackage package version
      for_ maybeOldVersion (installPackage package)
      pure $ Left err
    Right _ -> do
      FS.Extra.remove (backupWorkDir env)
      pure $ Right $ PackageSet $ case change of
        Remove -> set { packages = Map.delete package set.packages }
        Update version -> set { packages = Map.insert package version set.packages }

-- | Computes commit mesage for new package set publication.
-- | Note: The `PackageSet` argument is the old package set.
commitMessage :: PackageSet -> ChangeSet -> Version -> String
commitMessage (PackageSet set) accepted newVersion = String.joinWith "\n" $ fold
  [ [ "Release " <> Version.print newVersion <> " package set.\n" ]
  , guardA (not (Array.null added)) $> (addedLines <> "\n")
  , guardA (not (Array.null updated)) $> (updatedLines <> "\n")
  , guardA (not (Array.null removed)) $> (removedLines <> "\n")
  ]
  where
  added = do
    Tuple packageName change <- Map.toUnfoldable accepted
    version <- case change of
      Remove -> []
      Update version -> [ version ]
    guardA (not (Map.member packageName set.packages))
    pure $ Tuple packageName version

  addedLines = "New packages:\n" <> String.joinWith "\n" do
    Tuple packageName version <- added
    pure $ Array.fold [ "  - ", formatPackageVersion packageName version ]

  updated = do
    Tuple packageName change <- Map.toUnfoldable accepted
    version <- case change of
      Remove -> []
      Update version -> [ version ]
    previousVersion <- maybe [] pure (Map.lookup packageName set.packages)
    pure $ Tuple packageName { version, previousVersion }

  updatedLines = "Updated packages:\n" <> String.joinWith "\n" do
    Tuple packageName { version, previousVersion } <- updated
    pure $ Array.fold [ "  - ", formatPackageVersion packageName previousVersion, " -> ", Version.print version ]

  removed = do
    Tuple packageName change <- Map.toUnfoldable accepted
    guardA (change == Remove)
    version <- maybe [] pure (Map.lookup packageName set.packages)
    pure $ Tuple packageName version

  removedLines = "Removed packages:\n" <> String.joinWith "\n" do
    Tuple packageName version <- removed
    pure $ Array.fold [ "  - ", formatPackageVersion packageName version ]

-- | Computes new package set version from old package set and version information of successfully added/updated packages.
-- | Note: this must be called with the old `PackageSet` that has not had updates applied.
computeNewVersion :: Version -> PackageSet -> ChangeSet -> Version
computeNewVersion compiler (PackageSet set) changes = do
  let
    updates = changes # Map.mapMaybe case _ of
      Remove -> Nothing
      Update version -> Just version

  -- The compiler uses 0.MAJOR.MINOR versioning, so a minor version update
  -- corresponds with a major version package set.
  if (Version.minor compiler) > (Version.minor set.compiler) || Map.size updates < Map.size changes || isMajor updates then
    Version.bumpMajor set.version
  else if Version.patch compiler > Version.patch set.compiler || isMinor updates then
    Version.bumpMinor set.version
  else if isPatch updates then
    Version.bumpPatch set.version
  else
    set.version

  where
  -- Check for major version bumps for existing packages
  isMajor :: Map PackageName Version -> Boolean
  isMajor updates = Map.toUnfoldable updates # Array.any \(Tuple package version) -> fromMaybe false do
    prevVersion <- Map.lookup package set.packages
    pure (version >= Version.bumpMajor prevVersion)

  -- Check for minor version bumps for existing packages or package introductions
  isMinor :: Map PackageName Version -> Boolean
  isMinor updates = Map.toUnfoldable updates # Array.any \(Tuple package version) -> fromMaybe true do
    prevVersion <- Map.lookup package set.packages
    pure (version >= Version.bumpMinor prevVersion)

  -- Check for patch version bumps for existing packages
  isPatch :: Map PackageName Version -> Boolean
  isPatch updates = Map.toUnfoldable updates # Array.any \(Tuple package version) -> fromMaybe false do
    prevVersion <- Map.lookup package set.packages
    pure (version >= Version.bumpPatch prevVersion)

type ValidatedCandidates =
  { accepted :: Map PackageName (Maybe Version)
  , rejected :: Map PackageName { reason :: String, value :: Maybe Version }
  }

-- | Validate a package set is self-contained, ignoring version bounds.
validatePackageSet :: forall m. MonadRegistry m => MonadLog m => PackageSet -> ExceptT String m Unit
validatePackageSet (PackageSet set) = do
  Log.debug $ "Validating package set version " <> Version.print set.version
  index <- Except.ExceptT Registry.readAllManifests

  let
    printedVersion = Version.print set.version

    -- First, we need to associate manifests with each package in the set so
    -- we can verify their dependencies.
    manifests = do
      Tuple name version <- Map.toUnfoldable set.packages
      case ManifestIndex.lookup name version index of
        Nothing -> pure $ Left { name, version }
        Just manifest -> pure $ Right manifest

    -- No unregistered package versions are allowed in the package sets, so it
    -- should be impossible for the lookup to fail. Nevertheless, we need to
    -- verify that is in fact the case.
    { fail, success } = partitionEithers manifests

  -- If we failed to look up one or more manifests then the package versions are
  -- unregistered according to the index, and therefore the set is invalid.
  when (not (Array.null fail)) do
    Log.error $ Array.fold
      [ "Package set " <> printedVersion <> " is invalid because some package versions are not registered in the manifest index:"
      , Array.foldMap (\package -> "\n  - " <> formatPackageVersion package.name package.version) fail
      ]
    Except.throwError $ "Package set " <> printedVersion <> " is invalid because it includes unregistered package versions."

  let
    -- We can now attempt to produce a self-contained manifest index from the
    -- collected manifests. If this fails then the package set is not
    -- self-contained.
    Tuple unsatisfied _ = ManifestIndex.maximalIndex (Set.fromFoldable success)

  -- Otherwise, we can check if we were able to produce an index from the
  -- package set alone, without errors.
  unless (Map.isEmpty unsatisfied) do
    let
      failures = do
        Tuple name versions <- Map.toUnfoldable unsatisfied
        Tuple version dependencies <- Map.toUnfoldable versions
        [ { name, version, dependencies } ]

    Log.error $ Array.fold
      [ "Package set " <> printedVersion <> " is invalid because some package versions have unsatisfied dependencies:"
      , failures # Array.foldMap \package -> Array.fold
          [ "\n  - "
          , formatPackageVersion package.name package.version
          , "("
          , String.joinWith ", " (map PackageName.print (Array.fromFoldable (Map.keys package.dependencies)))
          , ")"
          ]
      ]

    Except.throwError $ "Package set " <> printedVersion <> " is invalid because some package versions have unsatisfied dependencies."

-- | Validate a provided set of package set candidates. Should be used before
-- | attempting to process a batch of packages for a package set.
validatePackageSetCandidates
  :: ManifestIndex
  -> PackageSet
  -> Map PackageName (Maybe Version)
  -> ValidatedCandidates
validatePackageSetCandidates index (PackageSet { packages: previousPackages }) candidates = do
  let { left: removalCandidates, right: updateCandidates } = Filterable.partitionMap (maybe (Left Nothing) Right) candidates
  let updates = validateUpdates updateCandidates
  -- We assume a batch either succeeds or fails atomically when it contains removals.
  -- Therefore we want to validate removals against the package set with accepted updates applied.
  let removals = validateRemovals (Map.catMaybes updates.accepted) (Map.keys removalCandidates)
  { accepted: Map.union updates.accepted removals.accepted
  , rejected: Map.union updates.rejected removals.rejected
  }
  where
  emptyValidated :: ValidatedCandidates
  emptyValidated = { accepted: Map.empty, rejected: Map.empty }

  validateUpdates :: Map PackageName Version -> ValidatedCandidates
  validateUpdates =
    flip foldlWithIndex emptyValidated \name acc version -> case validateUpdate name version of
      Left error ->
        acc { rejected = Map.insert name { reason: error, value: Just version } acc.rejected }
      Right _ ->
        acc { accepted = Map.insert name (Just version) acc.accepted }

  validateUpdate :: PackageName -> Version -> Either String Unit
  validateUpdate name version = do
    -- A package can't be "updated" to a lower version of a package that already
    -- exists in the package set.
    case Map.lookup name previousPackages of
      Nothing -> Right unit
      Just v | v < version -> Right unit
      Just v | v == version -> Left "This version already exists in the package set."
      Just v -> Left $ "A higher version already exists in the package set: " <> Version.print v

    -- A package can only be added to the package set if all its dependencies
    -- already exist in the package set or the batch being processed.
    let noManifestError = "No manifest entry exists in the registry index."
    Manifest manifest <- note noManifestError (ManifestIndex.lookup name version index)

    let
      dependencies = Array.fromFoldable (Map.keys manifest.dependencies)
      -- Note: A proper verification would topologically sort by dependencies
      -- and  track dropped packages as we go so we can transitively drop
      -- packages if a dependency in the batch is itself dropped due to a
      -- validation error.
      dependencyExists dependency = case Map.lookup dependency candidates of
        Nothing -> Map.member dependency previousPackages
        -- This indicates that the dependency is being removed.
        Just Nothing -> false
        Just (Just _) -> true

    case Array.filter (not <<< dependencyExists) dependencies of
      [] -> pure unit
      missing -> do
        let formatMissing = String.joinWith ", " <<< map PackageName.print
        Left $ "Missing dependencies (" <> formatMissing missing <> ")"

  validateRemovals :: Map PackageName Version -> Set PackageName -> ValidatedCandidates
  validateRemovals updates removals = do
    let updatedPackages = Map.union updates previousPackages
    removals # flip foldl emptyValidated \acc name -> case validateRemoval updatedPackages removals name of
      Left error ->
        acc { rejected = Map.insert name { reason: error, value: Nothing } acc.rejected }
      Right _ ->
        acc { accepted = Map.insert name Nothing acc.accepted }

  validateRemoval :: Map PackageName Version -> Set PackageName -> PackageName -> Either String Unit
  validateRemoval updatedPackages removals name = do
    -- A package can't be removed if it would cause other packages in the set to
    -- stop compiling, namely because they depend on this package. The
    -- dependents must also be removed if the removal must be processed.
    let otherPackages = Map.delete name updatedPackages
    case Array.filter (dependsOn name) (Map.toUnfoldable otherPackages) of
      [] -> pure unit
      dependents -> case Array.filter (fst >>> flip (not Set.member) removals) dependents of
        -- A package can be removed if the only packages that depend on it are also being removed.
        [] -> pure unit
        missing -> do
          let formatMissing = String.joinWith ", " <<< map (uncurry formatPackageVersion)
          Left $ "Has dependents that aren't also being removed (" <> formatMissing missing <> ")"

  dependsOn :: PackageName -> Tuple PackageName Version -> Boolean
  dependsOn removal (Tuple package version) = fromMaybe false do
    Manifest manifest <- ManifestIndex.lookup package version index
    pure $ Map.member removal manifest.dependencies

printRejections :: Map PackageName { reason :: String, value :: Maybe Version } -> String
printRejections rejections = do
  let sift { value, reason } = maybe (Left reason) (\version -> Right { reason, version }) value
  let { left, right } = Filterable.partitionMap sift rejections
  Array.fold
    [ Monoid.guard (not Map.isEmpty left) do
        let init = "Cannot be removed:"
        left # flip foldlWithIndex init \name acc reason ->
          acc <> "\n  - " <> printRemoval name reason
    , Monoid.guard (not Map.isEmpty right) do
        let init = "Cannot be added or updated:"
        right # flip foldlWithIndex init \name acc val ->
          acc <> "\n  - " <> printUpdate name val
    ]
  where
  printUpdate name { version, reason } =
    formatPackageVersion name version <> ": " <> reason

  printRemoval name reason =
    PackageName.print name <> ": " <> reason

logCompilerError :: forall m. MonadLog m => Version -> Purs.CompilerFailure -> ExceptT String m Unit
logCompilerError version = case _ of
  MissingCompiler -> do
    Log.error $ "Compilation failed because compiler " <> Version.print version <> " is missing."
    Except.throwError "Could not run compiler."
  UnknownError error -> do
    Log.error $ "Compilation failed because of an unknown error: " <> error
    Except.throwError "Could not run compiler."
  CompilationError errors -> do
    Log.info $ "Compilation failed with errors:\n" <> Purs.printCompilerErrors errors

updatePackageSetMetadata :: forall m. MonadEffect m => Version -> { previous :: PackageSet, pending :: PackageSet } -> ChangeSet -> m PackageSet
updatePackageSetMetadata compiler { previous, pending: PackageSet pending } changes = do
  now <- nowUTC
  let version = computeNewVersion compiler previous changes
  pure $ PackageSet (pending { compiler = compiler, version = version, published = DateTime.date now })
