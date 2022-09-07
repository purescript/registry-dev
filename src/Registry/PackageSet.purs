module Registry.PackageSet
  ( PackageSetBatchResult
  , commitMessage
  , getPackageSetPath
  , getPackageSetsPath
  , printRejections
  , processBatchAtomic
  , processBatchSequential
  , readLatestPackageSet
  , validatePackageSetCandidates
  ) where

import Registry.Prelude

import Affjax as Http
import Control.Alternative (guard)
import Control.Monad.Reader (asks)
import Data.Array as Array
import Data.Bitraversable (ltraverse)
import Data.Filterable (partitionMap)
import Data.Foldable (foldl, traverse_)
import Data.Map as Map
import Data.Monoid as Monoid
import Data.Set as Set
import Data.String as String
import Effect.Aff as Aff
import Effect.Now as Now
import Effect.Ref as Ref
import Foreign.Node.FS as FSE
import Foreign.Purs (CompilerFailure(..))
import Foreign.Purs as Purs
import Foreign.Tar as Tar
import Foreign.Wget as Wget
import Node.FS.Aff as FS.Aff
import Node.FS.Aff as FSA
import Node.Path as Path
import Registry.Constants as Constants
import Registry.Index (RegistryIndex)
import Registry.Json as Json
import Registry.PackageGraph as PackageGraph
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.RegistryM (RegistryM, throwWithComment)
import Registry.Schema (Manifest(..), PackageSet(..))
import Registry.Version (Version)
import Registry.Version as Version

getPackageSetsPath :: RegistryM FilePath
getPackageSetsPath = do
  registryPath <- asks _.registry
  pure $ Path.concat [ registryPath, Constants.packageSetsPath ]

getPackageSetPath :: Version -> RegistryM FilePath
getPackageSetPath version = do
  packageSets <- getPackageSetsPath
  pure $ Path.concat [ packageSets, Version.printVersion version <> ".json" ]

-- | Read the most recent package set release
readLatestPackageSet :: RegistryM PackageSet
readLatestPackageSet = do
  packageSetsPath <- getPackageSetsPath
  packageSetFiles <- liftAff $ FS.Aff.readdir packageSetsPath

  let
    packageSetVersions :: Array Version
    packageSetVersions = packageSetFiles # Array.mapMaybe \fileName -> do
      versionString <- String.stripSuffix (String.Pattern ".json") fileName
      hush $ Version.parseVersion Version.Lenient versionString

  case Array.last (Array.sort packageSetVersions) of
    Nothing -> throwWithComment "No existing package set."
    Just version -> do
      path <- getPackageSetPath version
      liftAff (Json.readJsonFile path) >>= case _ of
        Left err -> throwWithComment $ "Could not decode latest package set: " <> err
        Right set -> pure set

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
processBatchAtomic :: PackageSet -> Maybe Version -> Map PackageName (Maybe Version) -> RegistryM (Maybe PackageSetBatchResult)
processBatchAtomic prevSet@(PackageSet { compiler: prevCompiler, packages }) newCompiler batch = do
  let compilerVersion = fromMaybe prevCompiler newCompiler

  liftAff (installPackages packages *> compileInstalledPackages compilerVersion) >>= case _ of
    Left compilerError -> do
      handleCompilerError compilerVersion compilerError
      throwWithComment "Starting package set must compile in order to process a batch."
    Right _ -> pure unit

  liftAff (tryBatch compilerVersion prevSet batch) >>= case _ of
    Right newSet -> do
      packageSet <- liftEffect $ updatePackageSetMetadata { previous: prevSet, pending: newSet } batch
      pure $ Just { fail: Map.empty, success: batch, packageSet }
    Left batchCompilerError -> do
      handleCompilerError compilerVersion batchCompilerError
      pure Nothing

-- | Attempt to produce a new package set from the given package set by adding
-- | or removing the provided packages. Attempts the entire batch first, and
-- | then falls sequential processing if that fails.
processBatchSequential :: RegistryIndex -> PackageSet -> Maybe Version -> Map PackageName (Maybe Version) -> RegistryM (Maybe PackageSetBatchResult)
processBatchSequential registryIndex prevSet@(PackageSet { compiler: prevCompiler, packages }) newCompiler batch = do
  processBatchAtomic prevSet newCompiler batch >>= case _ of
    Just batchResult ->
      pure (Just batchResult)
    Nothing -> do
      -- If compiling the full batch failed, then we move on to processing
      -- packages one-by-one. To ensure the greatest likelihood of success, we
      -- sort packages by their dependencies.
      let
        compilerVersion = fromMaybe prevCompiler newCompiler
        sortedPackages = PackageGraph.topologicalSort registryIndex
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

      let
        formatName name version =
          PackageName.print name <> "@" <> Version.printVersion version

      -- Then we attempt to add them one-by-one.
      for_ sortedBatch \(Tuple name maybeVersion) -> do
        currentSet <- liftEffect $ Ref.read packageSetRef
        result <- liftAff (tryPackage currentSet name maybeVersion)
        case result of
          -- If the package could not be processed, then the state of the
          -- filesystem is rolled back. We just need to insert the package into
          -- the failures map to record that it could not be processed..
          Left packageCompilerError -> do
            liftEffect $ Ref.modify_ (Map.insert name maybeVersion) failRef
            log $ case maybeVersion of
              Nothing -> "Could not remove " <> PackageName.print name
              Just version -> "Could not add or update " <> formatName name version
            handleCompilerError compilerVersion packageCompilerError
          -- If the package could be processed, then the state of the filesystem
          -- is stepped and we need to record the success of this package and
          -- step the package set in memory for the next package to be processed
          -- with.
          Right newSet -> do
            liftEffect $ Ref.modify_ (Map.insert name maybeVersion) successRef
            liftEffect $ Ref.write newSet packageSetRef
            log $ case maybeVersion of
              Nothing -> "Removed " <> PackageName.print name
              Just version -> "Added or updated " <> formatName name version

      fail <- liftEffect $ Ref.read failRef
      success <- liftEffect $ Ref.read successRef
      newSet <- liftEffect $ Ref.read packageSetRef
      if Map.isEmpty success then pure Nothing
      else do
        packageSet <- liftEffect $ updatePackageSetMetadata { previous: prevSet, pending: newSet } success
        pure $ Just { fail, success, packageSet }

updatePackageSetMetadata :: { previous :: PackageSet, pending :: PackageSet } -> Map PackageName (Maybe Version) -> Effect PackageSet
updatePackageSetMetadata { previous, pending: PackageSet pending } changed = do
  now <- Now.nowDateTime
  let version = computeVersion previous changed
  pure $ PackageSet (pending { version = version, published = now })

handleCompilerError :: Version -> Purs.CompilerFailure -> RegistryM Unit
handleCompilerError compilerVersion = case _ of
  MissingCompiler ->
    throwWithComment $ "Missing compiler version " <> Version.printVersion compilerVersion
  UnknownError err ->
    throwWithComment $ "Unknown error: " <> err
  CompilationError errs -> do
    log "Compilation failed:\n"
    log $ Purs.printCompilerErrors errs <> "\n"

-- | Attempt to add, update, or delete a collection of packages in the package
-- | set. This will be rolled back if the operation fails.
-- |
-- | NOTE: You must have previously built a package set.
tryBatch :: Version -> PackageSet -> Map PackageName (Maybe Version) -> Aff (Either CompilerFailure PackageSet)
tryBatch compilerVersion (PackageSet set) batch = do
  let backupDir = "output-backup"
  let outputDir = "output"
  FSE.copy { from: outputDir, to: backupDir, preserveTimestamps: true }
  removePackages (Map.keys batch)
  installPackages (Map.catMaybes batch)
  compileInstalledPackages compilerVersion >>= case _ of
    Left err -> do
      FSE.remove outputDir
      FSE.copy { from: backupDir, to: outputDir, preserveTimestamps: true }
      for_ (Map.keys batch) \packageName -> do
        removePackage packageName
        for_ (Map.lookup packageName set.packages) (installPackage packageName)
      pure $ Left err
    Right _ -> do
      FSE.remove backupDir
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
tryPackage :: PackageSet -> PackageName -> Maybe Version -> Aff (Either CompilerFailure PackageSet)
tryPackage (PackageSet set) package maybeVersion = do
  let backupDir = "output-backup"
  let outputDir = "output"
  FSE.copy { from: outputDir, to: backupDir, preserveTimestamps: true }
  removePackage package
  for_ maybeVersion (installPackage package)
  compileInstalledPackages set.compiler >>= case _ of
    Left err -> do
      log $ case maybeVersion of
        Nothing -> "Failed to build set without " <> PackageName.print package
        Just version -> "Failed to build set with " <> PackageName.print package <> "@" <> Version.printVersion version
      FSE.remove outputDir
      FSE.copy { from: backupDir, to: outputDir, preserveTimestamps: true }
      for_ maybeVersion \_ -> removePackage package
      for_ (Map.lookup package set.packages) (installPackage package)
      pure $ Left err
    Right _ -> do
      FSE.remove backupDir
      pure $ Right $ PackageSet $ case maybeVersion of
        Nothing -> set { packages = Map.delete package set.packages }
        Just version -> set { packages = Map.insert package version set.packages }

-- | Compile all PureScript files in the given directory. Expects all packages
-- | to be installed in a subdirectory "packages".
compileInstalledPackages :: Version -> Aff (Either CompilerFailure String)
compileInstalledPackages compilerVersion = do
  log "Compiling installed packages..."
  let command = Purs.Compile { globs: [ "packages/**/*.purs" ] }
  let version = Version.printVersion compilerVersion
  Purs.callCompiler { command, version, cwd: Nothing }

-- | Delete package source directories in the given installation directory.
removePackages :: Set PackageName -> Aff Unit
removePackages = traverse_ removePackage

-- | Delete a package source directory in the given installation directory.
removePackage :: PackageName -> Aff Unit
removePackage name = FSE.remove (Path.concat [ "packages", PackageName.print name ])

-- | Install all packages in a package set into a temporary directory, returning
-- | the reference to the installation directory. Installed packages have the
-- | form: "package-name-major.minor.patch" and are stored in the "packages"
-- | directory.
installPackages :: Map PackageName Version -> Aff Unit
installPackages packages = do
  log "Installing packages..."
  FSE.ensureDirectory "packages"
  forWithIndex_ packages installPackage

-- | Install a package into the given installation directory, replacing an
-- | existing installation if there is on. Package sources are stored in the
-- | "packages" subdirectory of the given directory using their package name
-- | ie. 'aff'.
installPackage :: PackageName -> Version -> Aff Unit
installPackage name version = do
  log $ "installing " <> PackageName.print name <> "@" <> Version.printVersion version
  _ <- Wget.wget registryUrl tarballPath >>= ltraverse (Aff.error >>> throwError)
  liftEffect $ Tar.extract { cwd: packagesDir, archive: extractedName <> ".tar.gz" }
  FSE.remove tarballPath
  FSA.rename extractedPath installPath
  where
  packagesDir :: FilePath
  packagesDir = "packages"

  installPath :: FilePath
  installPath = Path.concat [ packagesDir, PackageName.print name ]

  extractedPath :: FilePath
  extractedPath = Path.concat [ packagesDir, extractedName ]

  tarballPath :: FilePath
  tarballPath = extractedPath <> ".tar.gz"

  extractedName :: String
  extractedName = PackageName.print name <> "-" <> Version.printVersion version

  registryUrl :: Http.URL
  registryUrl = Array.fold
    [ Constants.registryPackagesUrl
    , "/"
    , PackageName.print name
    , "/"
    , Version.printVersion version
    , ".tar.gz"
    ]

-- | Computes commit mesage for new package set publication.
-- | Note: The `PackageSet` argument is the old package set.
commitMessage :: PackageSet -> Map PackageName (Maybe Version) -> Version -> String
commitMessage (PackageSet set) accepted newVersion = String.joinWith "\n" $ fold
  [ [ "Release " <> Version.printVersion newVersion <> " package set.\n" ]
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
    pure $ Array.fold [ "  - ", PackageName.print packageName, "@", Version.printVersion version ]

  updated = do
    Tuple packageName maybeVersion <- Map.toUnfoldable accepted
    version <- maybe [] pure maybeVersion
    previousVersion <- maybe [] pure (Map.lookup packageName set.packages)
    pure $ Tuple packageName { version, previousVersion }

  updatedLines = "Updated packages:\n" <> String.joinWith "\n" do
    Tuple packageName { version, previousVersion } <- updated
    pure $ Array.fold [ "  - ", PackageName.print packageName, "@", Version.printVersion previousVersion, " -> ", Version.printVersion version ]

  removed = do
    Tuple packageName maybeVersion <- Map.toUnfoldable accepted
    guardA (isNothing maybeVersion)
    version <- maybe [] pure (Map.lookup packageName set.packages)
    pure $ Tuple packageName version

  removedLines = "Removed packages:\n" <> String.joinWith "\n" do
    Tuple packageName version <- removed
    pure $ Array.fold [ "  - ", PackageName.print packageName, "@", Version.printVersion version ]

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

-- | Validate a provided set of package set candidates. Should be used before
-- | attempting to process a batch of packages for a package set.
validatePackageSetCandidates
  :: RegistryIndex
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
      Just v -> Left $ "A higher version already exists in the package set: " <> Version.printVersion v

    -- A package can only be added to the package set if all its dependencies
    -- already exist in the package set or the batch being processed.
    let noManifestError = "No manifest entry exists in the registry index."
    Manifest manifest <- note noManifestError (Map.lookup version =<< Map.lookup name index)

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
      missing -> Left $ "Missing dependencies (" <> String.joinWith ", " (map PackageName.print missing) <> ")"

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
          let printPackage (Tuple packageName version) = String.joinWith "@" [ PackageName.print packageName, Version.printVersion version ]
          Left $ "Has dependents that aren't also being removed (" <> String.joinWith ", " (map printPackage missing) <> ")"

  dependsOn :: PackageName -> Tuple PackageName Version -> Boolean
  dependsOn removal (Tuple package version) = fromMaybe false do
    Manifest manifest <- Map.lookup version =<< Map.lookup package index
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
    Array.fold [ PackageName.print name, "@", Version.printVersion version, ": ", reason ]

  printRemoval name reason =
    Array.fold [ PackageName.print name, ": ", reason ]
