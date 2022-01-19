module Registry.API where

import Registry.Prelude

import Control.Monad.Except as Except
import Data.Argonaut.Parser as JsonParser
import Data.Array as Array
import Data.Generic.Rep as Generic
import Data.Map as Map
import Data.String as String
import Effect.Aff as Aff
import Effect.Exception (throw)
import Effect.Ref as Ref
import Foreign.Dhall as Dhall
import Foreign.GitHub (IssueNumber)
import Foreign.GitHub as GitHub
import Foreign.Object as Object
import Foreign.SemVer (SemVer)
import Foreign.SemVer as SemVer
import Foreign.Tar as Tar
import Foreign.Tmp as Tmp
import Node.Buffer as Buffer
import Node.ChildProcess as NodeProcess
import Node.Crypto.Hash as Hash
import Node.FS.Aff as FS
import Node.FS.Stats as FS.Stats
import Node.Process as Env
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.PackageUpload as Upload
import Registry.RegistryM (Env, RegistryM, closeIssue, comment, commitToTrunk, readPackagesMetadata, runRegistryM, throwWithComment, updatePackagesMetadata, uploadPackage)
import Registry.Schema (Manifest(..), Metadata, Operation(..), Repo(..), Target(..), addVersionToMetadata, isVersionInMetadata, mkNewMetadata)
import Registry.Scripts.LegacyImport.Error (ImportError(..))
import Registry.Scripts.LegacyImport.Manifest as Manifest
import Registry.Types (RawPackageName(..), RawVersion(..))
import Sunde as Process
import Text.Parsing.StringParser as StringParser

main :: Effect Unit
main = launchAff_ $ do
  eventPath <- liftEffect do
    Env.lookupEnv "GITHUB_EVENT_PATH"
      >>= maybe (throw "GITHUB_EVENT_PATH not defined in the environment") pure
  octokit <- liftEffect GitHub.mkOctokit
  packagesMetadata <- mkMetadataRef
  checkIndexExists

  readOperation eventPath >>= case _ of
    -- If the issue body is not just a JSON string, then we don't consider it
    -- to be an attempted operation and it is presumably just an issue on the
    -- registry repository.
    NotJson ->
      pure unit

    MalformedJson issue err -> runRegistryM (mkEnv octokit packagesMetadata issue) do
      comment $ Array.fold
        [ "The JSON input for this package update is malformed:"
        , newlines 2
        , "```" <> err <> "```"
        , newlines 2
        , "You can try again by commenting on this issue with a corrected payload."
        ]

    DecodedOperation issue op ->
      runRegistryM (mkEnv octokit packagesMetadata issue) (runOperation op)

data OperationDecoding
  = NotJson
  | MalformedJson IssueNumber String
  | DecodedOperation IssueNumber Operation

derive instance eqOperationDecoding :: Eq OperationDecoding
derive instance genericOperationDecoding :: Generic.Generic OperationDecoding _

instance showOperationDecoding :: Show OperationDecoding where
  show = genericShow

readOperation :: FilePath -> Aff OperationDecoding
readOperation eventPath = do
  fileContents <- FS.readTextFile UTF8 eventPath

  GitHub.Event { issueNumber, body } <- case Json.parseJson fileContents of
    Left err ->
      -- If we don't receive a valid event path or the contents can't be decoded
      -- then this is a catastrophic error and we exit the workflow.
      Aff.throwError $ Aff.error $ "Error while parsing json from " <> eventPath <> " : " <> err
    Right event ->
      pure event

  pure $ case JsonParser.jsonParser body of
    Left _err -> NotJson
    Right json -> case Json.decode json of
      Left err -> MalformedJson issueNumber err
      Right op -> DecodedOperation issueNumber op

-- TODO: test all the points where the pipeline could throw, to show that we are implementing
-- all the necessary checks

runOperation :: Operation -> RegistryM Unit
runOperation operation = case operation of
  -- TODO handle addToPackageSet
  Addition { packageName, fromBower, newRef, newPackageLocation } -> do
    -- check that we don't have a metadata file for that package
    ifM (liftAff $ FS.exists $ metadataFile packageName)
      -- if the metadata file already exists then we steer this to be an Update instead
      (runOperation $ Update { packageName, fromBower, updateRef: newRef })
      do
        addOrUpdate { packageName, fromBower, ref: newRef } $ mkNewMetadata newPackageLocation

  Update { packageName, fromBower, updateRef } -> do
    ifM (liftAff $ FS.exists $ metadataFile packageName)
      do
        metadata <- readPackagesMetadata >>= \packages -> case Map.lookup packageName packages of
          Nothing -> throwWithComment "Couldn't read metadata file for your package"
          Just m -> pure m
        addOrUpdate { packageName, fromBower, ref: updateRef } metadata
      (throwWithComment "Metadata file should exist. Did you mean to create an Addition?")

  Unpublish _ -> throwWithComment "Unpublish not implemented! Ask us for help!" -- TODO

metadataDir :: FilePath
metadataDir = "../metadata"

metadataFile :: PackageName -> FilePath
metadataFile packageName = metadataDir <> "/" <> PackageName.print packageName <> ".json"

indexDir :: FilePath
indexDir = "../registry-index"

addOrUpdate :: { fromBower :: Boolean, ref :: String, packageName :: PackageName } -> Metadata -> RegistryM Unit
addOrUpdate { ref, fromBower, packageName } metadata = do
  -- let's get a temp folder to do our stuffs
  tmpDir <- liftEffect $ Tmp.mkTmpDir
  -- fetch the repo and put it in the tempdir, returning the name of its toplevel dir
  folderName <- case metadata.location of
    Git _ -> do
      -- TODO: Support non-GitHub packages. Remember subdir when implementing this. (See #15)
      throwWithComment "Packages are only allowed to come from GitHub for now. See #15"
    GitHub { owner, repo, subdir } -> do
      -- TODO: Support subdir. In the meantime, we verify subdir is not present. (See #16)
      when (isJust subdir) $ throwWithComment "`subdir` is not supported for now. See #16"

      let tarballName = ref <> ".tar.gz"
      let absoluteTarballPath = tmpDir <> "/" <> tarballName
      let archiveUrl = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> tarballName
      log $ "Fetching tarball from GitHub: " <> archiveUrl
      wget archiveUrl absoluteTarballPath
      log $ "Tarball downloaded in " <> absoluteTarballPath
      liftEffect (Tar.getToplevelDir absoluteTarballPath) >>= case _ of
        Nothing ->
          throwWithComment "Could not find a toplevel dir in the tarball!"
        Just dir -> do
          log "Extracting the tarball..."
          liftEffect $ Tar.extract { cwd: tmpDir, filename: absoluteTarballPath }
          pure dir

  let absoluteFolderPath = tmpDir <> "/" <> folderName
  let manifestPath = absoluteFolderPath <> "/purs.json"
  log $ "Package extracted in " <> absoluteFolderPath

  -- If we're "importing from Bower" then we need to gather information about the
  -- legacy package and put all of that together into a Registry Manifest
  when fromBower do
    address <- case metadata.location of
      Git _ -> throwWithComment "Legacy packages can only come from GitHub. Aborting."
      GitHub { owner, repo } -> pure { owner, repo }

    semVer <- case SemVer.parseSemVer ref of
      Nothing -> throwWithComment $ "Not a valid SemVer version: " <> ref
      Just result -> pure result

    let
      liftError = map (lmap ManifestImportError)

      runManifest =
        Except.runExceptT <<< Except.mapExceptT (liftAff <<< map (lmap Json.printJson))

      gatherManifest :: ExceptT ImportError Aff Manifest
      gatherManifest = do
        manifestFields <- Manifest.constructManifestFields (RawPackageName $ show packageName) (RawVersion ref) address
        Except.mapExceptT liftError $ Manifest.toManifest packageName metadata.location semVer manifestFields

    runManifest gatherManifest >>= case _ of
      Left err ->
        throwWithComment $ "Unable to convert Bowerfile to a manifest: " <> err
      Right manifest ->
        liftAff $ Json.writeJsonFile manifestPath manifest

  -- Try to read the manifest, typechecking it
  manifest@(Manifest manifestRecord) <- liftAff (try $ FS.readTextFile UTF8 manifestPath) >>= case _ of
    Left _err -> throwWithComment $ "Manifest not found at " <> manifestPath
    Right manifestStr -> do
      liftAff (Dhall.jsonToDhallManifest manifestStr) >>= case _ of
        Left err ->
          throwWithComment $ "Could not type-check Manifest file: " <> err
        Right _ -> case Json.parseJson manifestStr of
          Left err -> throwWithComment $ "Could not convert Manifest to JSON: " <> err
          Right res -> pure res

  runChecks metadata manifest

  -- After we pass all the checks it's time to do side effects and register the package
  log "Packaging the tarball to upload..."
  -- We need the version number to upload the package
  let newVersion = manifestRecord.version
  let newDirname = PackageName.print packageName <> "-" <> SemVer.version newVersion
  liftAff $ FS.rename absoluteFolderPath (tmpDir <> "/" <> newDirname)
  let tarballPath = tmpDir <> "/" <> newDirname <> ".tar.gz"
  liftEffect $ Tar.create { cwd: tmpDir, folderName: newDirname, archiveName: tarballPath }
  log "Checking the tarball size..."
  FS.Stats.Stats { size: bytes } <- liftAff $ FS.stat tarballPath
  when (bytes > maxPackageBytes) do
    throwWithComment $ "Package tarball exceeds maximum size of " <> show maxPackageBytes <> " bytes."
  log "Hashing the tarball..."
  hash <- liftAff $ sha256sum tarballPath
  log $ "Hash: " <> hash
  log "Uploading package to the storage backend..."
  let uploadPackageInfo = { name: packageName, version: newVersion }
  uploadPackage uploadPackageInfo tarballPath
  log $ "Adding the new version " <> SemVer.version newVersion <> " to the package metadata file (hashes, etc)"
  log $ "Hash for ref " <> show ref <> " was " <> show hash
  let newMetadata = addVersionToMetadata newVersion { hash, ref, bytes } metadata
  let metadataFilePath = metadataFile packageName
  liftAff $ Json.writeJsonFile metadataFilePath newMetadata
  updatePackagesMetadata manifestRecord.name newMetadata
  commitToTrunk packageName metadataFilePath >>= case _ of
    Left _err ->
      comment "Package uploaded, but metadata not synced with the registry repository.\n\ncc @purescript/packaging"
    Right _ -> do
      comment "Package successfully uploaded to the registry! 🎉 🚀"
  closeIssue

  -- Optional steps below:
  -- TODO don't fail the pipeline if one of them fails

  -- Add to the registry index
  liftAff $ Index.insertManifest indexDir manifest

-- TODO: commit the registry-index

-- TODO: handle addToPackageSet: we'll try to add it to the latest set and build (see #156)
-- TODO: upload docs to pursuit (see #154)

runChecks :: Metadata -> Manifest -> RegistryM Unit
runChecks metadata (Manifest manifest) = do
  -- TODO: collect all errors and return them at once. Note: some of the checks
  -- are going to fail while parsing from JSON, so we should move them here if we
  -- want to handle everything together
  log "Running checks for the following manifest:"
  logShow manifest

  log "Checking that the Manifest includes the `lib` target"
  Target libTarget <- case Object.lookup "lib" manifest.targets of
    Nothing -> throwWithComment "Didn't find `lib` target in the Manifest!"
    Just a -> pure a

  log "Checking that `lib` target only includes `src`"
  when (libTarget.sources /= [ "src/**/*.purs" ]) do
    throwWithComment "The `lib` target only allows the following `sources`: `src/**/*.purs`"

  log "Check that version is unique"
  let prettyVersion = SemVer.version manifest.version
  case Object.lookup prettyVersion metadata.releases of
    Nothing -> pure unit
    Just info -> throwWithComment $ "You tried to upload a version that already exists: " <> show prettyVersion <> "\nIts metadata is: " <> show info

  log "Check that the version does not contain any build metadata"
  when (SemVer.build manifest.version /= []) do
    throwWithComment "Package version should not contain any build-metadata."

  log "Check that all dependencies are contained in the registry"
  packages <- readPackagesMetadata
  let lookupPackage = flip Map.lookup packages <=< (hush <<< PackageName.parse)
  let
    pkgNotInRegistry name = case lookupPackage name of
      Nothing -> Just name
      Just _p -> Nothing
  let pkgsNotInRegistry = Array.catMaybes $ map pkgNotInRegistry $ Object.keys libTarget.dependencies
  unless (Array.null pkgsNotInRegistry) do
    throwWithComment $ "Some dependencies of your package were not found in the Registry: " <> show pkgsNotInRegistry

sha256sum :: String -> Aff String
sha256sum filepath = do
  fileBuffer <- FS.readFile filepath
  liftEffect do
    newHash <- Hash.createHash "sha256"
    fileHash <- Hash.update fileBuffer newHash
    digest <- Hash.digest fileHash
    Buffer.toString Hex digest

wget :: String -> String -> RegistryM Unit
wget url path = do
  let cmd = "wget"
  let stdin = Nothing
  let args = [ "-O", path, url ]
  result <- liftAff $ Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  case result.exit of
    NodeProcess.Normally 0 -> pure unit
    _ -> throwWithComment $ "Error while fetching tarball: " <> result.stderr

mkEnv :: GitHub.Octokit -> Ref (Map PackageName Metadata) -> IssueNumber -> Env
mkEnv octokit packagesMetadata issue =
  { comment: GitHub.createComment octokit issue
  , closeIssue: GitHub.closeIssue octokit issue
  , commitToTrunk: pushToMaster
  , uploadPackage: Upload.upload
  , packagesMetadata
  }

type MetadataMap = Map PackageName Metadata
type MetadataRef = Ref MetadataMap

mkMetadataRef :: Aff MetadataRef
mkMetadataRef = do
  packageList <- try (FS.readdir metadataDir) >>= case _ of
    Right list -> pure $ Array.catMaybes $ map (String.stripSuffix $ String.Pattern ".json") list
    Left err -> do
      error $ show err
      FS.mkdir metadataDir
      pure []
  packagesArray <- for packageList \rawPackageName -> do
    packageName <- case PackageName.parse rawPackageName of
      Right p -> pure p
      Left err -> do
        log $ "Encountered error while parsing package name! It was: " <> rawPackageName
        Aff.throwError $ Aff.error $ StringParser.printParserError err
    let metadataPath = metadataFile packageName
    metadataStr <- FS.readTextFile UTF8 metadataPath
    metadata <- case Json.parseJson metadataStr of
      Left err -> Aff.throwError $ Aff.error $ "Error while parsing json from " <> metadataPath <> " : " <> err
      Right r -> pure r
    pure $ packageName /\ metadata
  liftEffect $ Ref.new $ Map.fromFoldable packagesArray

isPackageVersionInMetadata :: PackageName -> SemVer -> MetadataMap -> Boolean
isPackageVersionInMetadata packageName version metadataMap =
  case Map.lookup packageName metadataMap of
    Nothing -> false
    Just metadata -> isVersionInMetadata version metadata

checkIndexExists :: Aff Unit
checkIndexExists = do
  log $ "Checking if the registry-index is present.."

  whenM (not <$> FS.exists indexDir) do
    error "Didn't find the 'registry-index' repo, cloning..."
    (Except.runExceptT $ runGit [ "clone", "https://github.com/purescript/registry-index.git", indexDir ]) >>= case _ of
      Left err -> Aff.throwError $ Aff.error err
      Right _ -> log "Successfully cloned the 'registry-index' repo"

pushToMaster :: PackageName -> FilePath -> Aff (Either String Unit)
pushToMaster packageName path = Except.runExceptT do
  runGit [ "config", "user.name", "PacchettiBotti" ]
  runGit [ "config", "user.email", "<pacchettibotti@ferrai.io>" ]
  runGit [ "add", path ]
  runGit [ "commit", "-m", "Update metadata for package " <> PackageName.print packageName ]
  runGit [ "push", "origin", "master" ]

runGit :: Array String -> ExceptT String Aff Unit
runGit args = ExceptT do
  result <- Process.spawn { cmd: "git", args, stdin: Nothing } NodeProcess.defaultSpawnOptions
  case result.exit of
    NodeProcess.Normally 0 -> do
      info result.stdout
      info result.stderr
      pure $ Right unit
    _ -> pure $ Left result.stderr

maxPackageBytes :: Number
maxPackageBytes = 200_000.0
