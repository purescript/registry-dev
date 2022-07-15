module Registry.Scripts.PublishPackageSet where

import Registry.Prelude

import Affjax as Http
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
import Effect.Console as Console
import Effect.Exception (throw)
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
import Registry.API (CompilerFailure(..), callCompiler)
import Registry.API as API
import Registry.Cache as Cache
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageGraph as PackageGraph
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Prelude as Maybe
import Registry.RegistryM (Env, RegistryM, readPackagesMetadata, runRegistryM)
import Registry.Schema (Manifest(..), PackageSet(..))
import Registry.Version (Version)
import Registry.Version as Version

main :: Effect Unit
main = Aff.launchAff_ do
  _ <- Dotenv.loadFile

  liftEffect $ Console.log "Starting package set publishing..."

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
      , uploadPackage: mempty
      , deletePackage: mempty
      , octokit
      , cache
      , packagesMetadata: metadataRef
      , registry: "registry"
      , registryIndex: "registry-index"
      }

  runRegistryM env do
    API.fetchRegistryIndex
    API.fetchRegistry
    API.fillMetadataRef

    metadata <- readPackagesMetadata

    now <- liftEffect $ Now.nowDateTime

    let
      -- TODO: Use latest package's `publishedTime` to find new uploads.
      recentUploads :: Array (Tuple PackageName (NonEmptyArray Version))
      recentUploads = do
        Tuple packageName packageMetadata <- Map.toUnfoldable metadata
        let
          versions' :: Array Version
          versions' = do
            Tuple version { publishedTime } <- Map.toUnfoldable packageMetadata.published
            published <- maybe [] (pure <<< PDT.toDateTimeLossy) (PDT.fromRFC3339String publishedTime)
            let diff = DateTime.diff now published
            -- NOTE: Change this line for configurable lookback.
            guardA (diff <= Hours (Int.toNumber 240))
            pure version

        versions <- Array.fromFoldable (NonEmptyArray.fromArray versions')
        pure (Tuple packageName versions)

    liftEffect $ Console.log "Fetching latest package set..."

    registryPath <- asks _.registry
    packageSets <- liftAff $ FS.Aff.readdir (Path.concat [ registryPath, "package-sets" ])

    let
      packageSetVersions :: Array Version
      packageSetVersions = packageSets # Array.mapMaybe \s -> do
        let versionString = String.take (String.length s - 5) s
        hush $ Version.parseVersion Version.Lenient versionString

      latestPackageSet :: Maybe FilePath
      latestPackageSet = do
        latestVersion <- Array.last (Array.sort packageSetVersions)
        pure $ Path.concat
          [ "registry"
          , "package-sets"
          , Version.printVersion latestVersion <> ".json"
          ]

    packageSetPath :: FilePath <- case latestPackageSet of
      Nothing -> unsafeCrashWith "ERROR: No existing package set."
      Just packageSetPath -> pure packageSetPath

    packageSetResult :: Either String PackageSet <- liftAff $ Json.readJsonFile packageSetPath

    packageSet@(PackageSet { packages }) <- case packageSetResult of
      Left err -> unsafeCrashWith err
      Right packageSet -> pure packageSet

    liftEffect $ Console.log "Computing candidates for inclusion in package set..."

    let
      uploads :: Array (Tuple PackageName Version)
      uploads = do
        Tuple packageName versions' <- recentUploads
        -- We only care about the latest version
        let version = NonEmptyArray.last (NonEmptyArray.sort versions')
        -- Ensure package is not in package set, or latest version is newer than that in package set
        checkedVersion <- case Map.lookup packageName packages of
          Nothing -> pure version
          Just v | v < version -> pure version
          _ -> []
        pure (Tuple packageName checkedVersion)

    liftEffect $ Console.log "Found the following uploads eligible for inclusion in package set:"
    liftEffect $ Console.log (show uploads)

    result <- processBatch packageSet (Map.fromFoldable uploads)
    logShow result

type BatchResult =
  { fail :: Map PackageName Version
  , success :: Map PackageName Version
  , packageSet :: PackageSet
  }

-- | Attempt to produce a new package set from the given package set by adding
-- | the provided packages.
processBatch :: PackageSet -> Map PackageName Version -> RegistryM BatchResult
processBatch prevSet@(PackageSet { compiler }) batch = do
  tmp <- liftEffect Process.cwd

  let
    handleCompilerError = case _ of
      MissingCompiler -> throwError $ Aff.error $ "Missing compiler version " <> Version.printVersion compiler
      UnknownError err -> throwError $ Aff.error $ "Unknown error: " <> err
      CompilationError err -> log err

  liftAff (buildPackageSet tmp prevSet) >>= case _ of
    Left compilerError -> do
      handleCompilerError compilerError
      throwError $ Aff.error $ "Starting package set must compile in order to process a batch."
    Right _ -> pure unit

  -- First we attempt to add the entire batch.
  log "Compiling new batch..."
  liftAff (tryBatch tmp prevSet batch) >>= case _ of
    Right newSet -> pure { fail: Map.empty, success: batch, packageSet: newSet }
    Left batchCompilerError -> do
      log "Batch failed to process: \n"
      handleCompilerError batchCompilerError

      -- If compiling the full batch failed, then we move on to adding packages
      -- one-by-one. To ensure the greatest likelihood of success, we sort
      -- packages by their dependencies.
      registryIndexPath <- asks _.registryIndex
      registryIndex <- liftAff $ Index.readRegistryIndex registryIndexPath
      let sortedPackages = PackageGraph.topologicalSort registryIndex
      let sortedBatch = Array.filter (\(Manifest { name }) -> Maybe.isJust (Map.lookup name batch)) sortedPackages

      failRef <- liftEffect $ Ref.new Map.empty
      successRef <- liftEffect $ Ref.new Map.empty
      packageSetRef <- liftEffect $ Ref.new prevSet

      let formatName name version = PackageName.print name <> "@" <> Version.printVersion version

      -- Then we attempt to add them one-by-one.
      for_ sortedBatch \(Manifest { name, version }) -> do
        set <- liftEffect $ Ref.read packageSetRef
        result <- liftAff (tryPackage tmp set name version)
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

      liftEffect do
        fail <- Ref.read failRef
        success <- Ref.read successRef
        packageSet <- Ref.read packageSetRef
        pure { fail, success, packageSet }

-- | Attempt to add or update a collection of packages in the package set. This
-- | operation will be rolled back if the addition fails.
-- |
-- | NOTE: You must have previously built a package set with
-- | `buildPackageSet` before running this function.
tryBatch :: FilePath -> PackageSet -> Map PackageName Version -> Aff (Either CompilerFailure PackageSet)
tryBatch tmp (PackageSet set) packages = do
  let backupDir = Path.concat [ tmp, "output-backup" ]
  let outputDir = Path.concat [ tmp, "output" ]
  FSE.copy { from: outputDir, to: backupDir }
  installPackages tmp packages
  compileInstalledPackages tmp set.compiler >>= case _ of
    Left err -> do
      FSE.remove outputDir
      FSE.copy { from: backupDir, to: outputDir }
      removePackages tmp (Map.keys packages)
      pure $ Left err
    Right _ -> do
      FSE.remove backupDir
      pure $ Right $ PackageSet $ set { packages = Map.union packages set.packages }

-- | Attempt to add or update a package in the package set. This operation will
-- | be rolled back if the addition fails.
-- |
-- | NOTE: You must have previously built a package set with
-- | `buildPackageSet` before running this function.
tryPackage :: FilePath -> PackageSet -> PackageName -> Version -> Aff (Either CompilerFailure PackageSet)
tryPackage tmp (PackageSet set) package version = do
  let backupDir = Path.concat [ tmp, "output-backup" ]
  let outputDir = Path.concat [ tmp, "output" ]
  FSE.copy { from: outputDir, to: backupDir }
  installPackage tmp package version
  compileInstalledPackages tmp set.compiler >>= case _ of
    Left err -> do
      FSE.remove outputDir
      FSE.copy { from: backupDir, to: outputDir }
      removePackage tmp package
      pure $ Left err
    Right _ -> do
      FSE.remove backupDir
      pure $ Right $ PackageSet $ set { packages = Map.insert package version set.packages }

<<<<<<< HEAD
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

=======
-- | Install all packages in the given package set and compile them with the
-- | correct compiler version.
buildPackageSet :: FilePath -> PackageSet -> Aff (Either CompilerFailure String)
buildPackageSet tmp (PackageSet { compiler, packages }) = do
  log "Installing packages..."
  installPackages tmp packages
  log "Compiling package set..."
  compileInstalledPackages tmp compiler

-- | Compile all PureScript files in the given directory. Expects all packages
-- | to be installed in a subdirectory "packages".
compileInstalledPackages :: FilePath -> Version -> Aff (Either CompilerFailure String)
compileInstalledPackages tmp compilerVersion = do
  let args = [ "compile", "packages/**/*.purs" ]
  let version = Version.printVersion compilerVersion
  callCompiler { args, version, cwd: Just tmp }

-- | Delete package source directories in the given installation directory.
removePackages :: FilePath -> Set PackageName -> Aff Unit
removePackages tmp = traverse_ (removePackage tmp)

-- | Delete a package source directory in the given installation directory.
removePackage :: FilePath -> PackageName -> Aff Unit
removePackage tmp name =
  FSE.remove (Path.concat [ tmp, "packages", PackageName.print name ])

-- | Install all packages in a package set into a temporary directory, returning
-- | the reference to the installation directory. Installed packages have the
-- | form: "package-name-major.minor.patch" and are stored in the "packages"
-- | directory.
installPackages :: FilePath -> Map PackageName Version -> Aff Unit
installPackages tmp packages = do
  FSE.ensureDirectory $ Path.concat [ tmp, "packages" ]
  forWithIndex_ packages (installPackage tmp)

-- | Install a package into the given installation directory, replacing an
-- | existing installation if there is on. Package sources are stored in the
-- | "packages" subdirectory of the given directory using their package name
-- | ie. 'aff'.
installPackage :: FilePath -> PackageName -> Version -> Aff Unit
installPackage tmp name version = do
  log $ "installing " <> PackageName.print name <> "@" <> Version.printVersion version
  removePackage tmp name
  _ <- API.wget registryUrl tarballPath >>= ltraverse (Aff.error >>> throwError)
  liftEffect $ Tar.extract { cwd: packagesDir, filename: tarballPath }
  FSE.remove tarballPath
  FSA.rename extractedPath installPath
  where
  packagesDir :: FilePath
  packagesDir = Path.concat [ tmp, "packages" ]

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
>>>>>>> d40ff9f (Build package set)
