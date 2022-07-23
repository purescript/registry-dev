module Registry.Scripts.PublishPackageSet where

import Registry.Prelude

import Affjax as Http
import Control.Alternative (guard)
import Control.Monad.Reader (asks)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bitraversable (ltraverse)
import Data.DateTime as DateTime
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.Map as Map
import Data.PreciseDateTime as PDT
import Data.String as String
import Data.Time.Duration (Hours(..))
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Exception (throw)
import Effect.Exception as Exception
import Effect.Now as Now
import Effect.Ref as Ref
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Node.FS as FSE
import Foreign.Tar as Tar
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS.Aff
import Node.FS.Aff as FSA
import Node.Path as Path
import Node.Process as Node.Process
import Node.Process as Process
import Registry.API (CompilerFailure(..), MetadataMap, callCompiler)
import Registry.API as API
import Registry.Cache as Cache
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageGraph as PackageGraph
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.RegistryM (Env, RegistryM, commitPackageSetFile, readPackagesMetadata, runRegistryM)
import Registry.Schema (Manifest(..), PackageSet(..))
import Registry.Version (Version)
import Registry.Version as Version

data PublishMode = GeneratePackageSet | CommitPackageSet

derive instance Eq PublishMode

main :: Effect Unit
main = Aff.launchAff_ do
  _ <- Dotenv.loadFile

  log "Parsing CLI args..."
  mode <- liftEffect do
    args <- Array.drop 2 <$> Node.Process.argv
    case Array.uncons args of
      Nothing -> Exception.throw "Expected 'generate' or 'commit', but received no arguments."
      Just { head, tail: [] } -> case head of
        "generate" -> pure GeneratePackageSet
        "commit" -> pure CommitPackageSet
        other -> Exception.throw $ "Expected 'generate' or 'commit' but received: " <> other
      Just _ -> Exception.throw $ String.joinWith "\n"
        [ "Expected 'generate' or 'commit', but received multiple arguments:"
        , String.joinWith " " args
        ]

  log "Starting package set publishing..."

  githubToken <- liftEffect do
    Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  octokit <- liftEffect $ GitHub.mkOctokit githubToken
  cache <- Cache.useCache

  tmpDir <- liftEffect $ Tmp.mkTmpDir
  liftEffect $ Node.Process.chdir tmpDir

  metadataRef <- liftEffect $ Ref.new Map.empty

  let
    env :: Env
    env =
      { comment: \comment -> log ("[COMMENT] " <> comment)
      , closeIssue: mempty
      , commitMetadataFile: \_ _ -> pure (Right unit)
      , commitIndexFile: \_ _ -> pure (Right unit)
      , commitPackageSetFile: API.pacchettiBottiPushToRegistryPackageSets
      , uploadPackage: mempty
      , deletePackage: mempty
      , octokit
      , cache
      , username: mempty
      , packagesMetadata: metadataRef
      , registry: "registry"
      , registryIndex: "registry-index"
      }

  runRegistryM env do
    API.fetchRegistryIndex
    API.fetchRegistry
    API.fillMetadataRef

    metadata <- readPackagesMetadata

    log "Fetching latest package set..."

    registryPath <- asks _.registry
    packageSets <- liftAff $ FS.Aff.readdir (Path.concat [ registryPath, "package-sets" ])

    let
      buildPackageSetPath :: Version -> FilePath
      buildPackageSetPath version = Path.concat [ registryPath, "package-sets", Version.printVersion version <> ".json" ]

      packageSetVersions :: Array Version
      packageSetVersions = packageSets # Array.mapMaybe \s -> do
        let versionString = String.take (String.length s - 5) s
        hush $ Version.parseVersion Version.Lenient versionString

      latestPackageSetPath :: Maybe FilePath
      latestPackageSetPath = buildPackageSetPath <$> Array.last (Array.sort packageSetVersions)

    packageSetPath <- case latestPackageSetPath of
      Nothing -> unsafeCrashWith "ERROR: No existing package set."
      Just packageSetPath -> pure packageSetPath

    prevPackageSet@(PackageSet { packages }) <- liftAff (Json.readJsonFile packageSetPath) >>= case _ of
      Left err -> unsafeCrashWith err
      Right packageSet -> pure packageSet

    log "Computing candidates for inclusion in package set..."

    registryIndexPath <- asks _.registryIndex
    registryIndex <- liftAff $ Index.readRegistryIndex registryIndexPath

    candidates <- liftEffect $ computeCandidates registryIndex metadata packages

    if Map.isEmpty candidates then do
      log "No new package versions eligible for inclusion in the package set."
    else do
      let format name version = PackageName.print name <> "@" <> Version.printVersion version
      log "Found the following package versions eligible for inclusion in package set:"
      forWithIndex_ candidates (\name version -> log $ format name version)
      processBatch registryIndex prevPackageSet candidates >>= case _ of
        Nothing -> do
          log "\n----------\nNo packages could be added to the set. All packages failed:"
          forWithIndex_ candidates (\name version -> log $ format name version)
        Just { success, fail, packageSet } -> do
          unless (Map.isEmpty fail) do
            log "\n----------\nSome packages could not be added to the set:"
            forWithIndex_ fail (\name version -> log $ format name version)
          log "\n----------\nNew packages were added to the set!"
          forWithIndex_ success (\name version -> log $ format name version)
          let
            newVersion = (un PackageSet packageSet).version
            newPath = buildPackageSetPath newVersion
          liftAff $ Json.writeJsonFile newPath packageSet
          case mode of
            GeneratePackageSet ->
              pure unit
            CommitPackageSet ->
              commitPackageSetFile packageSet >>= case _ of
                Left err -> throwError $ Aff.error $ "Failed to commit package set file: " <> err
                Right _ -> pure unit

type BatchResult =
  { fail :: Map PackageName Version
  , success :: Map PackageName Version
  , packageSet :: PackageSet
  }

-- | Attempt to produce a new package set from the given package set by adding
-- | the provided packages.
processBatch :: RegistryIndex -> PackageSet -> Map PackageName Version -> RegistryM (Maybe BatchResult)
processBatch registryIndex prevSet@(PackageSet { compiler, packages }) batch = do
  let
    handleCompilerError = case _ of
      MissingCompiler ->
        throwError $ Aff.error $ "Missing compiler version " <> Version.printVersion compiler
      UnknownError err ->
        throwError $ Aff.error $ "Unknown error: " <> err
      CompilationError errs -> do
        log "Compilation failed:\n"
        log $ API.printCompilerErrors errs <> "\n"

  liftAff (installPackages packages *> compileInstalledPackages compiler) >>= case _ of
    Left compilerError -> do
      handleCompilerError compilerError
      throwError $ Aff.error $ "Starting package set must compile in order to process a batch."
    Right _ -> pure unit

  let
    updatePackageSetMetadata :: PackageSet -> Map PackageName Version -> RegistryM PackageSet
    updatePackageSetMetadata (PackageSet new) successes = do
      now <- liftEffect Now.nowDateTime
      let newVersion = computeVersion prevSet successes
      pure $ PackageSet $ new { version = newVersion, published = now }

  -- First we attempt to add the entire batch.
  log "Compiling new batch..."
  liftAff (tryBatch prevSet batch) >>= case _ of
    Right newSet -> do
      packageSet <- updatePackageSetMetadata newSet batch
      pure $ Just { fail: Map.empty, success: batch, packageSet }
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
            batchVersion <- Map.lookup name batch
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
  callCompiler { args, version, cwd: Nothing }

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
  _ <- API.wget registryUrl tarballPath >>= ltraverse (Aff.error >>> throwError)
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
    if major then
      Version.bumpMajor
    else if minor then
      Version.bumpMinor
    else if patch then
      Version.bumpPatch
    else
      identity

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

computeCandidates :: RegistryIndex -> MetadataMap -> Map PackageName Version -> Effect (Map PackageName Version)
computeCandidates registryIndex metadata previousPackageSet = do
  now <- Now.nowDateTime
  pure (validateDependencies (validateVersions (uploadCandidates now)))
  where
  uploadCandidates :: DateTime.DateTime -> Array (Tuple PackageName (NonEmptyArray Version))
  uploadCandidates now = do
    Tuple packageName packageMetadata <- Map.toUnfoldable metadata
    let
      versions' :: Array Version
      versions' = do
        Tuple version { publishedTime } <- Map.toUnfoldable packageMetadata.published
        published <- maybe [] (pure <<< PDT.toDateTimeLossy) (PDT.fromRFC3339String publishedTime)
        let diff = DateTime.diff now published
        -- NOTE: Change this line for configurable lookback.
        guardA (diff <= Hours (Int.toNumber 24))
        pure version

    versions <- Array.fromFoldable (NonEmptyArray.fromArray versions')
    pure (Tuple packageName versions)

  validateVersions :: Array (Tuple PackageName (NonEmptyArray Version)) -> Map PackageName Version
  validateVersions candidates = Map.fromFoldable do
    Tuple packageName versions' <- candidates
    -- We only care about the latest version
    let version = NonEmptyArray.last (NonEmptyArray.sort versions')
    -- Ensure package is not in package set, or latest version is newer than that in package set
    checkedVersion <- case Map.lookup packageName previousPackageSet of
      Nothing -> pure version
      Just v | v < version -> pure version
      _ -> []
    pure (Tuple packageName checkedVersion)

  -- A proper solution to this would be to topologically sort by dependencies
  -- and keep track of dropped packages as we go, to transitively drop packages based on missing versions.
  validateDependencies :: Map PackageName Version -> Map PackageName Version
  validateDependencies uploads = uploads # Map.filterWithKey \packageName version -> fromMaybe false do
    Manifest manifest <- Map.lookup packageName registryIndex >>= Map.lookup version

    let
      dependencies = Array.fromFoldable (Map.keys manifest.dependencies)
      -- A package can only be added to the package set if
      -- all of its dependencies are in the previous package set
      -- or in the current batch.
      checkDependency dependency =
        Map.member dependency previousPackageSet || Map.member dependency uploads

    pure $ Array.all checkDependency dependencies
