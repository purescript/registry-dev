-- | An effect for upgrading package sets, either in batch mode (all suggested
-- | changes must go in together) or in sequential mode (as many changes as
-- | possible will be added, and changes that would break the set are recorded.)
module Registry.App.Effect.PackageSets where

import Registry.App.Prelude

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
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Proxy (Proxy(..))

data Change = Update Version | Remove

derive instance Eq Change

type ChangeSet = Map PackageName Change

type SequentialUpgradeResult =
  { failed :: ChangeSet
  , succeeded :: ChangeSet
  , result :: PackageSet
  }

data PackageSets a
  = UpgradeAtomic PackageSet Version ChangeSet (Either String (Either String PackageSet) -> a)
  | UpgradeSequential PackageSet Version ChangeSet (Either String (Maybe SequentialUpgradeResult) -> a)

derive instance Functor PackageSets

-- | An effect for interacting with package sets
type PACKAGE_SETS r = (packageSets :: PackageSets | r)

_packageSets :: Proxy "packageSets"
_packageSets = Proxy

-- | Upgrade the given package set using the provided compiler version and set
-- | of changes. If any change fails, then the upgrade is aborted and the
-- | unsuccessful changes are returned.
upgradeAtomic :: forall r. PackageSet -> Version -> ChangeSet -> Run (PACKAGE_SETS + EXCEPT String + r) (Either String PackageSet)
upgradeAtomic oldSet compiler changes = Run.lift _packageSets (UpgradeAtomic oldSet compiler changes identity) >>= Except.rethrow

-- | Upgrade the given package set using the provided compiler version and set
-- | of changes. Any successful change is applied, and any unsuccessful changes
-- | are returned along with the new package set.
upgradeSequential :: forall r. PackageSet -> Version -> ChangeSet -> Run (PACKAGE_SETS + EXCEPT String + r) (Maybe SequentialUpgradeResult)
upgradeSequential oldSet compiler changes = do
  upgradeAtomic oldSet compiler changes >>= case _ of
    Left _ -> Run.lift _packageSets (UpgradeSequential oldSet compiler changes identity) >>= Except.rethrow
    Right result -> pure $ Just { failed: Map.empty, succeeded: changes, result }

interpret :: forall r a. (PackageSets ~> Run r) -> Run (PACKAGE_SETS + r) a -> Run r a
interpret handler = Run.interpret (Run.on _packageSets handler Run.send)

type PackageSetsEnv =
  { workdir :: FilePath
  }

-- | A handler for the PACKAGE_SETS effect which compiles the package sets and
-- | returns the results.
handle :: forall r a. PackageSetsEnv -> PackageSets a -> Run (REGISTRY + STORAGE + LOG + AFF + EFFECT + r) a
handle env = case _ of
  UpgradeAtomic oldSet@(PackageSet { packages }) compiler changes reply -> reply <$> Except.runExcept do
    Log.info $ "Performing atomic upgrade of package set " <> Version.print (un PackageSet oldSet).version

    -- It is possible to reuse a workdir when processing package set batches, so
    -- we need to clean up before doing work.
    for_ [ packagesWorkDir, outputWorkDir, backupWorkDir ] \dir -> do
      exists <- Run.liftEffect $ FS.Sync.exists dir
      when exists do
        Log.debug $ "Removing existing working directory " <> dir
        FS.Extra.remove dir

    installPackages packages
    compileInstalledPackages compiler >>= case _ of
      Left compilerError -> case compilerError of
        MissingCompiler -> Except.throw $ printMissingCompiler compiler
        UnknownError error -> Except.throw $ printUnknownError error
        CompilationError errors -> do
          Except.throw $ Array.fold
            [ "Compilation failed, but the starting package set must compile in order to process a batch:\n\n"
            , printCompilationError errors
            ]
      Right _ -> pure unit

    attemptChanges compiler oldSet changes >>= case _ of
      Left compilerError -> case compilerError of
        MissingCompiler -> Except.throw $ printMissingCompiler compiler
        UnknownError error -> Except.throw $ printUnknownError error
        CompilationError errors -> do
          let printed = printCompilationError errors
          Log.warn printed
          pure (Left printed)
      Right pending -> do
        newSet <- updatePackageSetMetadata compiler { previous: oldSet, pending } changes
        validatePackageSet newSet
        pure (Right newSet)

  UpgradeSequential oldSet@(PackageSet { packages }) compiler changes reply -> reply <$> Except.runExcept do
    Log.info $ "Performing sequential upgrade of package set " <> Version.print (un PackageSet oldSet).version
    index <- Registry.readAllManifests

    let sortedBatch = orderChanges index packages changes

    failRef <- Run.liftEffect $ Ref.new Map.empty
    successRef <- Run.liftEffect $ Ref.new Map.empty
    packageSetRef <- Run.liftEffect $ Ref.new oldSet

    for_ sortedBatch \(Tuple name change) -> do
      currentSet <- Run.liftEffect $ Ref.read packageSetRef
      attemptPackage compiler currentSet name change >>= case _ of
        -- If the package could not be processed, then the state of the
        -- filesystem is rolled back by attemptPackage. We just need to insert
        -- the package into the failures map.
        Left compilerError -> do
          Run.liftEffect $ Ref.modify_ (Map.insert name change) failRef
          Log.warn $ case change of
            Remove -> "Could not remove " <> PackageName.print name
            Update version -> "Could not add or update " <> formatPackageVersion name version
          case compilerError of
            MissingCompiler -> Except.throw $ printMissingCompiler compiler
            UnknownError error -> Except.throw $ printUnknownError error
            CompilationError errors -> Log.info $ printCompilationError errors

        -- If the package could be processed, then the state of the filesystem
        -- is stepped and we need to record the success of this package and
        -- step the package set in memory for the next package.
        Right newSet -> do
          Log.debug "Writing successful result and new package set to refs."
          Run.liftEffect do
            Ref.modify_ (Map.insert name change) successRef
            Ref.write newSet packageSetRef
          Log.info $ case change of
            Remove -> "Removed " <> PackageName.print name
            Update version -> "Added or updated " <> formatPackageVersion name version

    failed <- Run.liftEffect $ Ref.read failRef
    succeeded <- Run.liftEffect $ Ref.read successRef
    pending <- Run.liftEffect $ Ref.read packageSetRef

    case Map.size succeeded of
      0 -> pure $ Nothing
      _ -> do
        newSet <- updatePackageSetMetadata compiler { previous: oldSet, pending } succeeded
        validatePackageSet newSet
        pure $ Just { failed, succeeded, result: newSet }

  where
  packagesWorkDir :: FilePath
  packagesWorkDir = Path.concat [ env.workdir, "packages" ]

  outputWorkDir :: FilePath
  outputWorkDir = Path.concat [ env.workdir, "output" ]

  backupWorkDir :: FilePath
  backupWorkDir = Path.concat [ env.workdir, "output-backup" ]

  printMissingCompiler version = "Compilation failed because compiler " <> Version.print version <> " is missing."
  printUnknownError error = "Compilation failed because of an unknown error: " <> error
  printCompilationError errors = "Compilation failed with errors:\n" <> Purs.printCompilerErrors errors

  -- Install all packages in a package set into a temporary directory, returning
  -- the reference to the installation directory. Installed packages have the
  -- form: "package-name-major.minor.patch" and are stored in the "packages"
  -- directory.
  installPackages :: Map PackageName Version -> Run _ Unit
  installPackages packages = do
    Log.debug "Installing package set packages..."
    FS.Extra.ensureDirectory packagesWorkDir
    forWithIndex_ packages installPackage

  -- Install a package into the packages directory, skipping installation if a
  -- previous installation already exists.
  installPackage :: PackageName -> Version -> Run _ Unit
  installPackage name version = do
    let formattedName = formatPackageVersion name version
    let extractedName = PackageName.print name <> "-" <> Version.print version
    let tarballName = extractedName <> ".tar.gz"
    let tarballPath = Path.concat [ packagesWorkDir, tarballName ]
    let extractedPath = Path.concat [ packagesWorkDir, extractedName ]
    let installPath = Path.concat [ packagesWorkDir, formattedName ]
    unlessM (Run.liftEffect (FS.Sync.exists installPath)) $ do
      maybeMetadata <- Registry.readMetadata name
      case maybeMetadata >>= \(Metadata m) -> Map.lookup version m.published of
        Nothing -> Except.throw $ "No metadata found for " <> formattedName
        Just { hash, bytes } -> do
          Log.debug $ "Installing " <> formattedName
          Storage.download name version tarballPath { hash, bytes }
          Tar.extract { cwd: packagesWorkDir, archive: tarballName }
          FS.Extra.remove tarballPath
          Run.liftAff $ FS.Aff.rename extractedPath installPath

  -- Delete packages from the packages directory
  removePackages :: Map PackageName Version -> Run _ Unit
  removePackages = traverseWithIndex_ removePackage

  -- | Delete a package from the packages directory
  removePackage :: PackageName -> Version -> Run _ Unit
  removePackage name version = do
    let formatted = formatPackageVersion name version
    Log.debug $ "Uninstalling " <> formatted
    FS.Extra.remove $ Path.concat [ packagesWorkDir, formatted ]

  -- | Compile all PureScript files in the given directory. Expects all packages
  -- | to be installed in a subdirectory "packages".
  compileInstalledPackages :: Version -> Run _ (Either CompilerFailure String)
  compileInstalledPackages compiler = do
    Log.debug "Compiling installed packages..."
    let command = Purs.Compile { globs: [ Path.concat [ Path.basename packagesWorkDir, "**/*.purs" ] ] }
    Run.liftAff $ Purs.callCompiler { command, version: Just compiler, cwd: Just env.workdir }

  attemptChanges :: Version -> PackageSet -> ChangeSet -> Run _ (Either CompilerFailure PackageSet)
  attemptChanges compiler (PackageSet set) changes = do
    Log.debug "Attempting batch of changes..."
    FS.Extra.copy { from: outputWorkDir, to: backupWorkDir, preserveTimestamps: true }
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
        FS.Extra.remove outputWorkDir
        FS.Extra.copy { from: backupWorkDir, to: outputWorkDir, preserveTimestamps: true }
        removePackages additions
        installPackages oldVersions
        pure $ Left err
      Right _ -> do
        FS.Extra.remove backupWorkDir
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
  attemptPackage :: Version -> PackageSet -> PackageName -> Change -> Run _ (Either CompilerFailure PackageSet)
  attemptPackage compiler (PackageSet set) package change = do
    FS.Extra.copy { from: outputWorkDir, to: backupWorkDir, preserveTimestamps: true }
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
        FS.Extra.remove outputWorkDir
        FS.Extra.copy { from: backupWorkDir, to: outputWorkDir, preserveTimestamps: true }
        case change of
          Remove -> pure unit
          Update version -> removePackage package version
        for_ maybeOldVersion (installPackage package)
        pure $ Left err
      Right _ -> do
        FS.Extra.remove backupWorkDir
        pure $ Right $ PackageSet $ case change of
          Remove -> set { packages = Map.delete package set.packages }
          Update version -> set { packages = Map.insert package version set.packages }

-- | Computes commit mesage for new package set publication.
-- | Note: The `PackageSet` argument is the old package set.
commitMessage :: PackageSet -> ChangeSet -> Version -> String
commitMessage (PackageSet set) accepted newVersion = String.joinWith "\n" $ fold
  [ [ "Release " <> Version.print newVersion <> " package set.\n" ]
  , guard (not (Array.null added)) $> (addedLines <> "\n")
  , guard (not (Array.null updated)) $> (updatedLines <> "\n")
  , guard (not (Array.null removed)) $> (removedLines <> "\n")
  ]
  where
  added = do
    Tuple packageName change <- Map.toUnfoldable accepted
    version <- case change of
      Remove -> []
      Update version -> [ version ]
    guard (not (Map.member packageName set.packages))
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
    guard (change == Remove)
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
validatePackageSet :: forall r. PackageSet -> Run (REGISTRY + LOG + EXCEPT String + r) Unit
validatePackageSet (PackageSet set) = do
  Log.debug $ "Validating package set version " <> Version.print set.version
  index <- Registry.readAllManifests

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
    Except.throw $ "Package set " <> printedVersion <> " is invalid because it includes unregistered package versions."

  let
    -- We can now attempt to produce a self-contained manifest index from the
    -- collected manifests. If this fails then the package set is not
    -- self-contained.
    Tuple unsatisfied _ = ManifestIndex.maximalIndex ManifestIndex.IgnoreRanges (Set.fromFoldable success)

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

    Except.throw $ "Package set " <> printedVersion <> " is invalid because some package versions have unsatisfied dependencies."

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

updatePackageSetMetadata :: forall r. Version -> { previous :: PackageSet, pending :: PackageSet } -> ChangeSet -> Run (EFFECT + r) PackageSet
updatePackageSetMetadata compiler { previous, pending: PackageSet pending } changes = do
  now <- nowUTC
  let version = computeNewVersion compiler previous changes
  pure $ PackageSet (pending { compiler = compiler, version = version, published = DateTime.date now })

-- | Order a set of changes for sequential processing. Updates are processed in
-- | topological order (dependencies first), then removals are processed in
-- | reverse topological order (dependents first). This ensures:
-- | 1. Dependencies are updated before their dependents
-- | 2. Dependents are removed before their dependencies
-- |
-- | Updates are processed before removals because updates can enable removals
-- | (by removing dependencies on packages being removed), but removals never
-- | enable updates. For example, if A depends on B and both are in the change
-- | set where A is updated (to no longer depend on B) and B is removed, then
-- | A must be updated first so B's removal doesn't fail due to A's dependency.
orderChanges :: ManifestIndex -> Map PackageName Version -> ChangeSet -> Array (Tuple PackageName Change)
orderChanges index packages changes = sortedUpdates <> sortedRemovals
  where
  sortedPackages = ManifestIndex.toSortedArray ManifestIndex.IgnoreRanges index

  -- Updates should be processed in topological order (dependencies first)
  -- so that dependencies are updated before their dependents.
  sortedUpdates = sortedPackages # Array.mapMaybe \(Manifest { name, version }) -> case Map.lookup name changes of
    Just (Update updateVersion) | version == updateVersion -> Just (Tuple name (Update version))
    _ -> Nothing

  -- Removals should be processed in reverse topological order (dependents
  -- first) so that dependents are removed before their dependencies.
  sortedRemovals = sortedPackages # Array.reverse # Array.mapMaybe \(Manifest { name, version }) ->
    case Map.lookup name changes, Map.lookup name packages of
      Just Remove, Just prevVersion | version == prevVersion -> Just (Tuple name Remove)
      _, _ -> Nothing
