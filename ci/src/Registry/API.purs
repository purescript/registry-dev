module Registry.API where

import Registry.Prelude

import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT)
import Data.Argonaut (decodeJson, jsonParser, printJsonDecodeError)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Generic.Rep as Generic
import Data.Map as Map
import Data.String as String
import Effect.Aff as Aff
import Effect.Ref as Ref
import Foreign.Dhall as Dhall
import Foreign.GitHub (IssueNumber)
import Foreign.GitHub as GitHub
import Foreign.Object as Object
import Foreign.SemVer as SemVer
import Foreign.Tar as Tar
import Foreign.Tmp as Tmp
import Node.Buffer as Buffer
import Node.ChildProcess as NodeProcess
import Node.Crypto.Hash as Hash
import Node.FS.Aff as FS
import Node.Process as Env
import Partial.Unsafe (unsafePartial)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.PackageUpload as Upload
import Registry.RegistryM (Env, RegistryM, closeIssue, comment, commitToTrunk, readPackagesMetadata, runRegistryM, throwWithComment, updatePackagesMetadata, uploadPackage)
import Registry.Schema (Manifest, Metadata, Operation(..), Repo(..), addVersionToMetadata, mkNewMetadata)
import Registry.Scripts.BowerImport (toManifest)
import Registry.Scripts.BowerImport.Error (printManifestError)
import Sunde as Process
import Text.Parsing.StringParser as Parser
import Web.Bower.PackageMeta as Bower


main :: Effect Unit
main = launchAff_ $ do
  eventPath <- liftEffect $ Env.lookupEnv "GITHUB_EVENT_PATH"
  packagesMetadata <- do
    packageList <- try (FS.readdir metadataDir) >>= case _ of
      Right list -> pure list
      Left err -> do
        error $ show err
        FS.mkdir metadataDir
        pure []
    packagesArray <- for packageList \rawPackageName -> do
      packageName <- case PackageName.parse rawPackageName of
        Right p -> pure p
        Left err -> Aff.throwError $ Aff.error $ Parser.printParserError err
      let metadataPath = metadataFile packageName
      metadataStr <- FS.readTextFile UTF8 metadataPath
      metadata <- case fromJson metadataStr of
        Left err -> Aff.throwError $ Aff.error $ "Error while parsing json from " <> metadataPath <> " : " <> err
        Right r -> pure r
      pure $ packageName /\ metadata
    liftEffect $ Ref.new $ Map.fromFoldable packagesArray
  readOperation (unsafePartial fromJust eventPath) >>= case _ of
    -- If the issue body is not just a JSON string, then we don't consider it
    -- to be an attempted operation and it is presumably just an issue on the
    -- registry repository.
    NotJson ->
      pure unit

    MalformedJson issue err -> runRegistryM (mkEnv packagesMetadata issue) do
      comment $ Array.fold
        [ "The JSON input for this package update is malformed:"
        , newlines 2
        , "```" <> err <> "```"
        , newlines 2
        , "You can try again by commenting on this issue with a corrected payload."
        ]

    DecodedOperation issue op ->
      runRegistryM (mkEnv packagesMetadata issue) (runOperation op)

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

  GitHub.Event { issueNumber, body } <- case fromJson fileContents of
    Left err ->
      -- If we don't receive a valid event path or the contents can't be decoded
      -- then this is a catastrophic error and we exit the workflow.
      Aff.throwError $ Aff.error $ "Error while parsing json from " <> eventPath <> " : " <> err
    Right event ->
      pure event

  pure $ case Json.jsonParser body of
    Left _err ->
      NotJson
    Right json -> case Json.decodeJson json of
      Left err -> MalformedJson issueNumber (Json.printJsonDecodeError err)
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

  -- If we're importing from Bower then we need to convert the Bowerfile
  -- to a Registry Manifest
  when fromBower do
    liftAff (readBowerfile (absoluteFolderPath <> "/bower.json")) >>= case _ of
      Left err ->
         throwWithComment $ "Error while reading Bowerfile: " <> err
      Right bowerfile -> do
        let
          name =
            PackageName.print packageName

          printErrors =
            String.joinWith ", " <<< map printManifestError <<< NEA.toArray

          runManifest =
            runExceptT <<< mapExceptT (liftAff <<< map (lmap printErrors))

        runManifest (toManifest name metadata.location ref bowerfile) >>= case _ of
          Left err ->
            throwWithComment $ "Unable to convert Bowerfile to a manifest: " <> err
          Right manifest ->
            liftAff $ writeJsonFile manifestPath manifest

  -- Try to read the manifest, typechecking it
  manifest :: Manifest <- liftAff (try $ FS.readTextFile UTF8 manifestPath) >>= case _ of
    Left _err -> throwWithComment $ "Manifest not found at " <> manifestPath
    Right manifestStr -> do
      liftAff (Dhall.jsonToDhallManifest manifestStr) >>= case _ of
        Left err ->
          throwWithComment $ "Could not type-check Manifest file: " <> err
        Right _ -> case fromJson manifestStr of
          Left err -> throwWithComment $ "Could not convert Manifest to JSON: " <> err
          Right res -> pure res

  runChecks metadata manifest

  -- After we pass all the checks it's time to do side effects and register the package
  log "Packaging the tarball to upload..."
  -- We need the version number to upload the package
  let newVersion = manifest.version
  let newDirname = PackageName.print packageName <> "-" <> SemVer.printSemVer newVersion
  liftAff $ FS.rename absoluteFolderPath (tmpDir <> "/" <> newDirname)
  let tarballPath = tmpDir <> "/" <> newDirname <> ".tar.gz"
  liftEffect $ Tar.create { cwd: tmpDir, folderName: newDirname, archiveName: tarballPath }
  log "Hashing the tarball..."
  hash <- liftAff $ sha256sum tarballPath
  log $ "Hash: " <> hash
  log "Uploading package to the storage backend..."
  let uploadPackageInfo = { name: packageName, version: newVersion }
  uploadPackage uploadPackageInfo tarballPath
  log $ "Adding the new version " <> SemVer.printSemVer newVersion <> " to the package metadata file (hashes, etc)"
  log $ "Hash for ref " <> show ref <> " was " <> show hash
  let newMetadata = addVersionToMetadata newVersion { hash, ref } metadata
  let metadataFilePath = metadataFile packageName
  liftAff $ FS.writeTextFile UTF8 metadataFilePath (Json.stringifyWithIndent 2 $ Json.encodeJson newMetadata)
  updatePackagesMetadata manifest.name newMetadata
  commitToTrunk packageName metadataFilePath >>= case _ of
    Left _err ->
      comment "Package uploaded, but metadata not synced with the registry repository.\n\ncc @purescript/packaging"
    Right _ -> do
      comment "Package successfully uploaded to the registry! :tada: :rocket:"
  closeIssue
-- Optional steps that we'll try and that won't fail the pipeline on error:
-- TODO: handle addToPackageSet: we'll try to add it to the latest set and build (see #156)
-- TODO: upload docs to pursuit (see #154)

runChecks :: Metadata -> Manifest -> RegistryM Unit
runChecks metadata manifest = do
  -- TODO: collect all errors and return them at once. Note: some of the checks
  -- are going to fail while parsing from JSON, so we should move them here if we
  -- want to handle everything together

  log "Checking that the Manifest includes the `lib` target"
  libTarget <- case Object.lookup "lib" manifest.targets of
    Nothing -> throwWithComment "Didn't find `lib` target in the Manifest!"
    Just a -> pure a

  log "Checking that `lib` target only includes `src`"
  when (libTarget.sources /= [ "src/**/*.purs" ]) do
    throwWithComment "The `lib` target only allows the following `sources`: `src/**/*.purs`"

  log "Check that version is unique"
  let prettyVersion = SemVer.printSemVer manifest.version
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

fromJson :: forall a. Json.DecodeJson a => String -> Either String a
fromJson = Json.jsonParser >=> (lmap Json.printJsonDecodeError <<< Json.decodeJson)

sha256sum :: String -> Aff String
sha256sum filepath = do
  fileBuffer <- FS.readFile filepath
  liftEffect do
    newHash <- Hash.createHash Hash.SHA256
    fileHash <- Hash.update newHash fileBuffer
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

mkEnv :: Ref (Map PackageName Metadata) -> IssueNumber -> Env
mkEnv packagesMetadata issue =
  { comment: GitHub.createComment issue
  , closeIssue: GitHub.closeIssue issue
  , commitToTrunk: pushToMaster
  , uploadPackage: Upload.upload
  , packagesMetadata
  }

pushToMaster :: PackageName -> FilePath -> Aff (Either String Unit)
pushToMaster packageName path = runExceptT do
  runGit [ "config", "user.name", "PacchettiBotti" ]
  runGit [ "config", "user.email", "<pacchettibotti@ferrai.io>" ]
  runGit [ "add", path ]
  runGit [ "commit", "-m", "Update metadata for package " <> PackageName.print packageName ]
  runGit [ "push", "origin", "master" ]

  where
  runGit args = ExceptT do
    result <- Process.spawn { cmd: "git", args, stdin: Nothing } NodeProcess.defaultSpawnOptions
    case result.exit of
      NodeProcess.Normally 0 -> do
        info result.stdout
        info result.stderr
        pure $ Right unit
      _ -> pure $ Left result.stderr

readBowerfile :: String -> Aff (Either String Bower.PackageMeta)
readBowerfile path = do
  let fromJson' = jsonParser >=> (decodeJson >>> lmap printJsonDecodeError)
  ifM (not <$> FS.exists path)
    (pure $ Left $ "Bowerfile not found at " <> path)
    do
      strResult <- FS.readTextFile UTF8 path
      pure $ fromJson' strResult
