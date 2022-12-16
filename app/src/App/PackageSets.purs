module Registry.App.PackageSets
  ( PackageSetBatchResult
  , commitMessage
  , getPackageSetPath
  , getPackageSetsPath
  , printRejections
  , processBatchAtomic
  , processBatchSequential
  , readLatestPackageSet
  , validatePackageSet
  , validatePackageSetCandidates
  ) where

import Registry.App.Prelude

import Affjax as Http
import Control.Alternative (guard)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Reader as ReaderT
import Data.Array as Array
import Data.DateTime as DateTime
import Data.Filterable (partitionMap)
import Data.Foldable (foldl)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map as Map
import Data.Monoid as Monoid
import Data.Set as Set
import Data.String as String
import Data.Tuple (uncurry)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.FS.Aff as FS.Aff
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Registry.App.CLI.Purs (CompilerFailure(..))
import Registry.App.CLI.Purs as Purs
import Registry.App.CLI.Tar as Tar
import Registry.App.CLI.Wget as Wget
import Registry.App.RegistryM (RegistryM, throwWithComment)
import Registry.Constants as Constants
import Registry.Foreign.FSExtra as FS.Extra
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.Version as Version

getPackageSetsPath :: RegistryM FilePath
getPackageSetsPath = do
  registryPath <- asks _.registry
  pure $ Path.concat [ registryPath, Constants.packageSetsDirectory ]

getPackageSetPath :: Version -> RegistryM FilePath
getPackageSetPath version = do
  packageSets <- getPackageSetsPath
  pure $ Path.concat [ packageSets, Version.print version <> ".json" ]

-- | Read the most recent package set release
readLatestPackageSet :: RegistryM PackageSet
readLatestPackageSet = do
  packageSetsPath <- getPackageSetsPath
  packageSetFiles <- liftAff $ FS.Aff.readdir packageSetsPath

  let
    packageSetVersions :: Array Version
    packageSetVersions = packageSetFiles # Array.mapMaybe \fileName -> do
      versionString <- String.stripSuffix (String.Pattern ".json") fileName
      hush $ Version.parse versionString

  case Array.last (Array.sort packageSetVersions) of
    Nothing -> throwWithComment "No existing package set."
    Just version -> do
      path <- getPackageSetPath version
      liftAff (readJsonFile PackageSet.codec path) >>= case _ of
        Left err -> throwWithComment $ "Could not decode latest package set: " <> err
        Right set -> pure set

type Paths =
  { workDirectory :: FilePath
  , packagesDirectory :: FilePath
  , outputDirectory :: FilePath
  , outputBackupDirectory :: FilePath
  }

runWithPaths :: forall m a. MonadAff m => FilePath -> ReaderT Paths Aff a -> m a
runWithPaths workDir k = liftAff $ ReaderT.runReaderT k do
  { workDirectory: workDir
  , packagesDirectory: Path.concat [ workDir, "packages" ]
  , outputDirectory: Path.concat [ workDir, "output" ]
  , outputBackupDirectory: Path.concat [ workDir, "output-backup" ]
  }

-- TODO: It would be ideal to report _why_ a package failed, similar to the
-- batch validation.
type PackageSetBatchResult =
  { fail :: Map PackageName (Maybe Version)
  , success :: Map PackageName (Maybe Version)
  , packageSet :: PackageSet
  }

-- | Attempt to produce a new package set from the given package set by adding
-- | or removing the provided packages. Fails if the batch is not usable as a
-- | whole. To fall back to sequential processing, use `processBatchSequential`.
processBatchAtomic :: FilePath -> ManifestIndex -> PackageSet -> Maybe Version -> Map PackageName (Maybe Version) -> RegistryM (Maybe PackageSetBatchResult)
processBatchAtomic workDir index prevSet@(PackageSet { compiler: prevCompiler, packages }) newCompiler batch = do
  let
    compilerVersion = fromMaybe prevCompiler newCompiler
    buildInitialSet = runWithPaths workDir do
      paths <- ReaderT.ask
      liftAff do
        -- It's possible to run batches in a persistent directory, especially for
        -- testing purposes, so we always clean out the working directories we use.
        FS.Extra.remove paths.packagesDirectory
        FS.Extra.remove paths.outputDirectory
        FS.Extra.remove paths.outputBackupDirectory
      installPackages packages
      compileInstalledPackages compilerVersion

  buildInitialSet >>= case _ of
    Left compilerError -> do
      handleCompilerError compilerVersion compilerError
      throwWithComment "Starting package set must compile in order to process a batch."
    Right _ -> pure unit

  runWithPaths workDir (tryBatch compilerVersion prevSet batch) >>= case _ of
    Right newSet -> do
      packageSet <- liftEffect $ updatePackageSetMetadata { previous: prevSet, pending: newSet } batch
      validatePackageSet index packageSet
      pure $ Just { fail: Map.empty, success: batch, packageSet }
    Left batchCompilerError -> do
      handleCompilerError compilerVersion batchCompilerError
      pure Nothing

-- | Attempt to produce a new package set from the given package set by adding
-- | or removing the provided packages. Attempts the entire batch first, and
-- | then falls sequential processing if that fails.
processBatchSequential :: FilePath -> ManifestIndex -> PackageSet -> Maybe Version -> Map PackageName (Maybe Version) -> RegistryM (Maybe PackageSetBatchResult)
processBatchSequential workDir registryIndex prevSet@(PackageSet { compiler: prevCompiler, packages }) newCompiler batch = do
  processBatchAtomic workDir registryIndex prevSet newCompiler batch >>= case _ of
    Just batchResult ->
      pure (Just batchResult)
    Nothing -> do
      -- If compiling the full batch failed, then we move on to processing
      -- packages one-by-one. To ensure the greatest likelihood of success, we
      -- sort packages by their dependencies.
      let
        compilerVersion = fromMaybe prevCompiler newCompiler
        sortedPackages = ManifestIndex.toSortedArray registryIndex
        sortedBatch =
          sortedPackages # Array.mapMaybe \(Manifest { name, version }) -> do
            update <- Map.lookup name batch
            case update of
              Nothing -> do
                prevVersion <- Map.lookup name packages
                guard (version == prevVersion)
                pure (Tuple name Nothing)
              Just batchVersion -> do
                guard (version == batchVersion)
                pure (Tuple name (Just version))

      failRef <- liftEffect $ Ref.new Map.empty
      successRef <- liftEffect $ Ref.new Map.empty
      packageSetRef <- liftEffect $ Ref.new prevSet

      -- Then we attempt to add them one-by-one.
      for_ sortedBatch \(Tuple name maybeVersion) -> do
        currentSet <- liftEffect $ Ref.read packageSetRef
        result <- runWithPaths workDir $ tryPackage currentSet name maybeVersion
        case result of
          -- If the package could not be processed, then the state of the
          -- filesystem is rolled back. We just need to insert the package into
          -- the failures map to record that it could not be processed..
          Left packageCompilerError -> do
            liftEffect $ Ref.modify_ (Map.insert name maybeVersion) failRef
            Console.log $ case maybeVersion of
              Nothing -> "Could not remove " <> PackageName.print name
              Just version -> "Could not add or update " <> formatPackage name version
            handleCompilerError compilerVersion packageCompilerError
          -- If the package could be processed, then the state of the filesystem
          -- is stepped and we need to record the success of this package and
          -- step the package set in memory for the next package to be processed
          -- with.
          Right newSet -> do
            liftEffect $ Ref.modify_ (Map.insert name maybeVersion) successRef
            liftEffect $ Ref.write newSet packageSetRef
            Console.log $ case maybeVersion of
              Nothing -> "Removed " <> PackageName.print name
              Just version -> "Added or updated " <> formatPackage name version

      fail <- liftEffect $ Ref.read failRef
      success <- liftEffect $ Ref.read successRef
      newSet <- liftEffect $ Ref.read packageSetRef
      if Map.isEmpty success then pure Nothing
      else do
        packageSet <- liftEffect $ updatePackageSetMetadata { previous: prevSet, pending: newSet } success
        pure $ Just { fail, success, packageSet }

updatePackageSetMetadata :: { previous :: PackageSet, pending :: PackageSet } -> Map PackageName (Maybe Version) -> Effect PackageSet
updatePackageSetMetadata { previous, pending: PackageSet pending } changed = do
  now <- nowUTC
  let version = computeVersion previous changed
  pure $ PackageSet (pending { version = version, published = DateTime.date now })

handleCompilerError :: Version -> Purs.CompilerFailure -> RegistryM Unit
handleCompilerError compilerVersion = case _ of
  MissingCompiler ->
    throwWithComment $ "Missing compiler version " <> Version.print compilerVersion
  UnknownError err ->
    throwWithComment $ "Unknown error: " <> err
  CompilationError errs -> do
    Console.log "Compilation failed:\n"
    Console.log $ Purs.printCompilerErrors errs <> "\n"

-- | Attempt to add, update, or delete a collection of packages in the package
-- | set. This will be rolled back if the operation fails.
-- |
-- | NOTE: You must have previously built a package set.
tryBatch :: Version -> PackageSet -> Map PackageName (Maybe Version) -> ReaderT Paths Aff (Either CompilerFailure PackageSet)
tryBatch compilerVersion (PackageSet set) batch = do
  paths <- ReaderT.ask
  liftAff $ FS.Extra.copy { from: paths.outputDirectory, to: paths.outputBackupDirectory, preserveTimestamps: true }
  let
    batchAdditions = Map.catMaybes batch
    oldVersions = Map.fromFoldable do
      Tuple name _ <- Map.toUnfoldable batch
      case Map.lookup name set.packages of
        Nothing -> []
        Just version -> [ Tuple name version ]
  removePackages oldVersions
  installPackages batchAdditions
  compileInstalledPackages compilerVersion >>= case _ of
    Left err -> do
      liftAff $ FS.Extra.remove paths.outputDirectory
      liftAff $ FS.Extra.copy { from: paths.outputBackupDirectory, to: paths.outputDirectory, preserveTimestamps: true }
      removePackages batchAdditions
      installPackages oldVersions
      pure $ Left err
    Right _ -> do
      liftAff $ FS.Extra.remove paths.outputBackupDirectory
      let
        foldFn name existingSet = case _ of
          Nothing -> Map.delete name existingSet
          Just version -> Map.insert name version existingSet
        newSet = foldlWithIndex foldFn set.packages batch
      pure $ Right $ PackageSet $ set { packages = newSet }

-- | Attempt to add, update, or remove a package in the package set. This
-- | operation will be rolled back if the addition fails.
-- |
-- | NOTE: You must have previously built a package set.
tryPackage :: PackageSet -> PackageName -> Maybe Version -> ReaderT Paths Aff (Either CompilerFailure PackageSet)
tryPackage (PackageSet set) package maybeNewVersion = do
  paths <- ReaderT.ask
  liftAff $ FS.Extra.copy { from: paths.outputDirectory, to: paths.outputBackupDirectory, preserveTimestamps: true }
  let maybeOldVersion = Map.lookup package set.packages
  for_ maybeOldVersion (removePackage package)
  for_ maybeNewVersion (installPackage package)
  compileInstalledPackages set.compiler >>= case _ of
    Left err -> do
      Console.log $ case maybeNewVersion of
        Nothing -> "Failed to build set without " <> PackageName.print package
        Just version -> "Failed to build set with " <> formatPackage package version
      liftAff do
        FS.Extra.remove paths.outputDirectory
        FS.Extra.copy { from: paths.outputBackupDirectory, to: paths.outputDirectory, preserveTimestamps: true }
      for_ maybeNewVersion (removePackage package)
      for_ maybeOldVersion (installPackage package)
      pure $ Left err
    Right _ -> do
      liftAff $ FS.Extra.remove paths.outputBackupDirectory
      pure $ Right $ PackageSet $ case maybeNewVersion of
        Nothing -> set { packages = Map.delete package set.packages }
        Just version -> set { packages = Map.insert package version set.packages }

-- | Compile all PureScript files in the given directory. Expects all packages
-- | to be installed in a subdirectory "packages".
compileInstalledPackages :: Version -> ReaderT Paths Aff (Either CompilerFailure String)
compileInstalledPackages compilerVersion = do
  paths <- ReaderT.ask
  Console.log "Compiling installed packages..."
  let command = Purs.Compile { globs: [ Path.concat [ Path.basename paths.packagesDirectory, "**/*.purs" ] ] }
  let version = Version.print compilerVersion
  liftAff $ Purs.callCompiler { command, version, cwd: Just paths.workDirectory }

-- | Delete package source directories in the given installation directory.
removePackages :: Map PackageName Version -> ReaderT Paths Aff Unit
removePackages = traverseWithIndex_ removePackage

-- | Delete a package source directory in the given installation directory.
removePackage :: PackageName -> Version -> ReaderT Paths Aff Unit
removePackage name version = do
  paths <- ReaderT.ask
  let formattedPackage = formatPackage name version
  Console.log $ "Uninstalling " <> formattedPackage
  liftAff $ FS.Extra.remove $ Path.concat [ paths.packagesDirectory, formattedPackage ]

-- | Install all packages in a package set into a temporary directory, returning
-- | the reference to the installation directory. Installed packages have the
-- | form: "package-name-major.minor.patch" and are stored in the "packages"
-- | directory.
installPackages :: Map PackageName Version -> ReaderT Paths Aff Unit
installPackages packages = do
  paths <- ReaderT.ask
  Console.log "Installing packages..."
  liftAff $ FS.Extra.ensureDirectory paths.packagesDirectory
  forWithIndex_ packages installPackage

-- | Install a package into the given installation directory, replacing an
-- | existing installation if there is on. Package sources are stored in the
-- | "packages" subdirectory of the given directory using their package name
-- | ie. 'aff'.
installPackage :: PackageName -> Version -> ReaderT Paths Aff Unit
installPackage name version = do
  paths <- ReaderT.ask
  let
    formattedPackage = formatPackage name version
    extractedName = PackageName.print name <> "-" <> Version.print version
    tarballPath = Path.concat [ paths.packagesDirectory, extractedName <> ".tar.gz" ]
    extractedPath = Path.concat [ paths.packagesDirectory, extractedName ]
    installPath = Path.concat [ paths.packagesDirectory, formattedPackage ]
  unlessM (liftEffect (FS.Sync.exists installPath)) $ liftAff do
    Console.log $ "Installing " <> formattedPackage
    _ <- Wget.wget registryUrl tarballPath >>= ltraverse (Aff.error >>> throwError)
    liftEffect $ Tar.extract { cwd: paths.packagesDirectory, archive: extractedName <> ".tar.gz" }
    FS.Extra.remove tarballPath
    FS.Aff.rename extractedPath installPath
  where
  registryUrl :: Http.URL
  registryUrl = Array.fold
    [ Constants.packageStorageUrl
    , "/"
    , PackageName.print name
    , "/"
    , Version.print version
    , ".tar.gz"
    ]

-- | Computes commit mesage for new package set publication.
-- | Note: The `PackageSet` argument is the old package set.
commitMessage :: PackageSet -> Map PackageName (Maybe Version) -> Version -> String
commitMessage (PackageSet set) accepted newVersion = String.joinWith "\n" $ fold
  [ [ "Release " <> Version.print newVersion <> " package set.\n" ]
  , guardA (not (Array.null added)) $> (addedLines <> "\n")
  , guardA (not (Array.null updated)) $> (updatedLines <> "\n")
  , guardA (not (Array.null removed)) $> (removedLines <> "\n")
  ]
  where
  added = do
    Tuple packageName maybeVersion <- Map.toUnfoldable accepted
    version <- maybe [] pure maybeVersion
    guardA (not (Map.member packageName set.packages))
    pure $ Tuple packageName version

  addedLines = "New packages:\n" <> String.joinWith "\n" do
    Tuple packageName version <- added
    pure $ Array.fold [ "  - ", formatPackage packageName version ]

  updated = do
    Tuple packageName maybeVersion <- Map.toUnfoldable accepted
    version <- maybe [] pure maybeVersion
    previousVersion <- maybe [] pure (Map.lookup packageName set.packages)
    pure $ Tuple packageName { version, previousVersion }

  updatedLines = "Updated packages:\n" <> String.joinWith "\n" do
    Tuple packageName { version, previousVersion } <- updated
    pure $ Array.fold [ "  - ", formatPackage packageName previousVersion, " -> ", Version.print version ]

  removed = do
    Tuple packageName maybeVersion <- Map.toUnfoldable accepted
    guardA (isNothing maybeVersion)
    version <- maybe [] pure (Map.lookup packageName set.packages)
    pure $ Tuple packageName version

  removedLines = "Removed packages:\n" <> String.joinWith "\n" do
    Tuple packageName version <- removed
    pure $ Array.fold [ "  - ", formatPackage packageName version ]

-- | Computes new package set version from old package set and version information of successfully added/updated packages.
-- | Note: this must be called with the old `PackageSet` that has not had updates applied.
computeVersion :: PackageSet -> Map PackageName (Maybe Version) -> Version
computeVersion (PackageSet set) changes = do
  let updates = Map.catMaybes changes
  -- Removals always require a major version bump.
  if Map.size updates < Map.size changes || isMajor updates then
    Version.bumpMajor set.version
  else if isMinor updates then
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
validatePackageSet :: ManifestIndex -> PackageSet -> RegistryM Unit
validatePackageSet index (PackageSet set) = do
  let
    errorPrefix = "Package set " <> Version.print set.version <> " is invalid!\n"

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
    let
      failedMessages = fail <#> \package -> Array.fold
        [ "  - "
        , PackageName.print package.name
        , "@"
        , Version.print package.version
        ]

    throwWithComment $ String.joinWith "\n"
      [ errorPrefix
      , "Some package versions in the package set are not registered:"
      , String.joinWith "\n" failedMessages
      ]

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

      failedMessages = failures <#> \package -> Array.fold
        [ "  - "
        , PackageName.print package.name
        , "@"
        , Version.print package.version
        , " ("
        , String.joinWith ", " (map PackageName.print (Array.fromFoldable (Map.keys package.dependencies)))
        , ")."
        ]

    throwWithComment $ String.joinWith "\n"
      [ errorPrefix
      , "Some package versions in the set have unsatisfied dependencies:"
      , String.joinWith "\n" failedMessages
      ]

-- | Validate a provided set of package set candidates. Should be used before
-- | attempting to process a batch of packages for a package set.
validatePackageSetCandidates
  :: ManifestIndex
  -> PackageSet
  -> Map PackageName (Maybe Version)
  -> ValidatedCandidates
validatePackageSetCandidates index (PackageSet { packages: previousPackages }) candidates = do
  let { left: removalCandidates, right: updateCandidates } = partitionMap (maybe (Left Nothing) Right) candidates
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
          let formatMissing = String.joinWith ", " <<< map (uncurry formatPackage)
          Left $ "Has dependents that aren't also being removed (" <> formatMissing missing <> ")"

  dependsOn :: PackageName -> Tuple PackageName Version -> Boolean
  dependsOn removal (Tuple package version) = fromMaybe false do
    Manifest manifest <- ManifestIndex.lookup package version index
    pure $ Map.member removal manifest.dependencies

printRejections :: Map PackageName { reason :: String, value :: Maybe Version } -> String
printRejections rejections = do
  let sift { value, reason } = maybe (Left reason) (\version -> Right { reason, version }) value
  let { left, right } = partitionMap sift rejections
  Array.fold
    [ Monoid.guard (not Map.isEmpty left) do
        let init = "Cannot be removed:\n"
        left # flip foldlWithIndex init \name acc reason ->
          acc <> "  - " <> printRemoval name reason <> "\n"
    , Monoid.guard (not Map.isEmpty right) do
        let init = "Cannot be added or updated:\n"
        right # flip foldlWithIndex init \name acc val ->
          acc <> "  - " <> printUpdate name val <> "\n"
    ]
  where
  printUpdate name { version, reason } =
    formatPackage name version <> ": " <> reason

  printRemoval name reason =
    PackageName.print name <> ": " <> reason

formatPackage :: PackageName -> Version -> FilePath
formatPackage name version = PackageName.print name <> "@" <> Version.print version
