module Registry.PackageSet
  ( PackageSetBatchResult
  , getPackageSetPath
  , getPackageSetsPath
  , printRejections
  , processBatch
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
  pure $ Path.concat [ registryPath, "package-sets" ]

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
  { fail :: Map PackageName Version
  , success :: Map PackageName Version
  , packageSet :: PackageSet
  }

-- TODO: Removals (ie. Map PackageName (Maybe Version))

-- | Attempt to produce a new package set from the given package set by adding
-- | the provided packages.
processBatch :: RegistryIndex -> PackageSet -> Maybe Version -> Map PackageName Version -> RegistryM (Maybe PackageSetBatchResult)
processBatch registryIndex prevSet@(PackageSet { compiler: prevCompiler, packages }) newCompiler newPackages = do
  let
    compilerVersion = fromMaybe prevCompiler newCompiler

    handleCompilerError = case _ of
      MissingCompiler ->
        throwWithComment $ "Missing compiler version " <> Version.printVersion compilerVersion
      UnknownError err ->
        throwWithComment $ "Unknown error: " <> err
      CompilationError errs -> do
        log "Compilation failed:\n"
        log $ Purs.printCompilerErrors errs <> "\n"

  liftAff (installPackages packages *> compileInstalledPackages compilerVersion) >>= case _ of
    Left compilerError -> do
      handleCompilerError compilerError
      throwWithComment "Starting package set must compile in order to process a batch."
    Right _ -> pure unit

  let
    updatePackageSetMetadata :: PackageSet -> Map PackageName Version -> RegistryM PackageSet
    updatePackageSetMetadata (PackageSet new) successes = do
      now <- liftEffect Now.nowDateTime
      let newVersion = computeVersion prevSet successes
      pure $ PackageSet $ new { version = newVersion, published = now }

  -- First we attempt to add the entire batch.
  log "Compiling new batch..."
  liftAff (tryBatch prevSet newPackages) >>= case _ of
    Right newSet -> do
      packageSet <- updatePackageSetMetadata newSet newPackages
      pure $ Just { fail: Map.empty, success: newPackages, packageSet }
    Left batchCompilerError -> do
      log "Batch failed to process: \n"
      handleCompilerError batchCompilerError

      -- If compiling the full batch failed, then we move on to adding packages
      -- one-by-one. To ensure the greatest likelihood of success, we sort
      -- packages by their dependencies.
      let
        sortedPackages = PackageGraph.topologicalSort registryIndex
        sortedBatch =
          sortedPackages # Array.filter \(Manifest { name, version }) -> fromMaybe false do
            batchVersion <- Map.lookup name newPackages
            guard $ version == batchVersion
            pure true

      failRef <- liftEffect $ Ref.new Map.empty
      successRef <- liftEffect $ Ref.new Map.empty
      packageSetRef <- liftEffect $ Ref.new prevSet

      let formatName name version = PackageName.print name <> "@" <> Version.printVersion version

      -- Then we attempt to add them one-by-one.
      for_ sortedBatch \(Manifest { name, version }) -> do
        currentSet <- liftEffect $ Ref.read packageSetRef
        result <- liftAff (tryPackage currentSet name version)
        case result of
          -- If the package could not be added, then the state of the filesystem
          -- is rolled back. We just need to insert the package into the
          -- failures map to record that it could not be added.
          Left packageCompilerError -> do
            liftEffect $ Ref.modify_ (Map.insert name version) failRef
            log $ "Could not add " <> formatName name version
            handleCompilerError packageCompilerError
          -- If the package could be added, then the state of the filesystem is
          -- stepped and we need to record the success of this package and step
          -- the package set in memory for the next package to be added to.
          Right newSet -> do
            liftEffect $ Ref.modify_ (Map.insert name version) successRef
            liftEffect $ Ref.write newSet packageSetRef
            log $ "Added " <> formatName name version

      fail <- liftEffect $ Ref.read failRef
      success <- liftEffect $ Ref.read successRef
      newSet <- liftEffect $ Ref.read packageSetRef
      if Map.isEmpty success then pure Nothing
      else do
        packageSet <- updatePackageSetMetadata newSet success
        pure $ Just { fail, success, packageSet }

-- | Attempt to add or update a collection of packages in the package set. This
-- | operation will be rolled back if the addition fails.
-- |
-- | NOTE: You must have previously built a package set.
tryBatch :: PackageSet -> Map PackageName Version -> Aff (Either CompilerFailure PackageSet)
tryBatch (PackageSet set) packages = do
  let backupDir = "output-backup"
  let outputDir = "output"
  FSE.copy { from: outputDir, to: backupDir, preserveTimestamps: true }
  removePackages (Map.keys packages)
  installPackages packages
  compileInstalledPackages set.compiler >>= case _ of
    Left err -> do
      FSE.remove outputDir
      FSE.copy { from: backupDir, to: outputDir, preserveTimestamps: true }
      for_ (Map.keys packages) \packageName -> do
        removePackage packageName
        for_ (Map.lookup packageName set.packages) (installPackage packageName)
      pure $ Left err
    Right _ -> do
      FSE.remove backupDir
      pure $ Right $ PackageSet $ set { packages = Map.union packages set.packages }

-- | Attempt to add or update a package in the package set. This operation will
-- | be rolled back if the addition fails.
-- |
-- | NOTE: You must have previously built a package set.
tryPackage :: PackageSet -> PackageName -> Version -> Aff (Either CompilerFailure PackageSet)
tryPackage (PackageSet set) package version = do
  let backupDir = "output-backup"
  let outputDir = "output"
  FSE.copy { from: outputDir, to: backupDir, preserveTimestamps: true }
  removePackage package
  installPackage package version
  compileInstalledPackages set.compiler >>= case _ of
    Left err -> do
      log $ "Failed to build set with " <> PackageName.print package <> "@" <> Version.printVersion version
      FSE.remove outputDir
      FSE.copy { from: backupDir, to: outputDir, preserveTimestamps: true }
      removePackage package
      for_ (Map.lookup package set.packages) (installPackage package)
      pure $ Left err
    Right _ -> do
      FSE.remove backupDir
      pure $ Right $ PackageSet $ set { packages = Map.insert package version set.packages }

-- | Compile all PureScript files in the given directory. Expects all packages
-- | to be installed in a subdirectory "packages".
compileInstalledPackages :: Version -> Aff (Either CompilerFailure String)
compileInstalledPackages compilerVersion = do
  log "Compiling installed packages..."
  let args = [ "compile", "packages/**/*.purs" ]
  let version = Version.printVersion compilerVersion
  Purs.callCompiler { args, version, cwd: Nothing }

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
  liftEffect $ Tar.extract { cwd: packagesDir, filename: tarballPath }
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
    [ "https://packages.registry.purescript.org/"
    , PackageName.print name
    , "/"
    , Version.printVersion version
    , ".tar.gz"
    ]

-- | Computes new package set version from old package set and version information of successfully added/updated packages.
-- | Note: this must be called with the old `PackageSet` that has not had updates applied.
computeVersion :: PackageSet -> Map PackageName Version -> Version
computeVersion (PackageSet { packages, version: packageSetVersion }) updates =
  updateVersion packageSetVersion
  where
  updateVersion =
    if major then Version.bumpMajor
    else if minor then Version.bumpMinor
    else if patch then Version.bumpPatch
    else identity

  -- Check for major version bumps for existing packages
  major :: Boolean
  major = Map.toUnfoldable updates # Array.any \(Tuple package version) -> fromMaybe false do
    prevVersion <- Map.lookup package packages
    pure (version >= Version.bumpMajor prevVersion)

  -- Check for minor version bumps for existing packages or package introductions
  minor :: Boolean
  minor = Map.toUnfoldable updates # Array.any \(Tuple package version) -> fromMaybe true do
    prevVersion <- Map.lookup package packages
    pure (version >= Version.bumpMinor prevVersion)

  -- Check for patch version bumps for existing packages
  patch :: Boolean
  patch = Map.toUnfoldable updates # Array.any \(Tuple package version) -> fromMaybe false do
    prevVersion <- Map.lookup package packages
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
  let removals = validateRemovals (Map.keys removalCandidates)
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
      missing -> Left $ "Missing dependencies: " <> String.joinWith ", " (map PackageName.print missing)

  validateRemovals :: Set PackageName -> ValidatedCandidates
  validateRemovals =
    flip foldl emptyValidated \acc name -> case validateRemoval name of
      Left error ->
        acc { rejected = Map.insert name { reason: error, value: Nothing } acc.rejected }
      Right _ ->
        acc { accepted = Map.insert name Nothing acc.accepted }

  -- FIXME: TODO: Does this need to have work done in PackageGraph?
  validateRemoval :: PackageName -> Either String Unit
  validateRemoval name = do
    -- A package can't be removed if it would cause other packages in the set to
    -- stop compiling, namely because they depend on this package. The
    -- dependents must also be removed if the removal must be processed.
    pure unit

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
