module Registry.API where

import Prelude

import Data.Argonaut as Json
import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Generic.Rep as Generic
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Show.Generic (genericShow)
import Data.Traversable (all, for)
import Data.Tuple.Nested ((/\))
import Dhall as Dhall
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Foreign.Object as Object
import GitHub (IssueNumber)
import GitHub as GitHub
import Node.Buffer as Buffer
import Node.ChildProcess as NodeProcess
import Node.Crypto.Hash as Hash
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)
import Node.Process as Env
import PackageUpload as PackageUpload
import Partial.Unsafe (unsafePartial)
import Registry.BowerImport as Bower
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.RegistryM (RegistryM, comment, mkEnv, readPackagesMetadata, runRegistryM, throwWithComment)
import Registry.Schema (Manifest, Operation(..), Repo(..), Revision, Metadata)
import Registry.Utils as Utils
import Registry.Version (Version)
import Registry.Version as Version
import Sunde as Process
import Tar as Tar
import Test.Spec.Assertions as Assert
import Text.Parsing.StringParser as Parser
import Tmp as Tmp

main :: Effect Unit
main = launchAff_ $ do
  eventPath <- liftEffect $ Env.lookupEnv "GITHUB_EVENT_PATH"
  packagesMetadata <- do
    whenM (not <$> FS.exists metadataDir) do
      FS.mkdir metadataDir
    packageList <- FS.readdir metadataDir
    packagesArray <- for packageList \rawPackageName -> do
      packageName <- case PackageName.parse rawPackageName of
        Right p -> pure p
        Left err -> Aff.throwError $ Aff.error $ Parser.printParserError err
      metadata <- readJsonFile $ metadataFile packageName
      pure $ packageName /\ metadata
    liftEffect $ Ref.new $ Map.fromFoldable packagesArray
  readOperation (unsafePartial fromJust eventPath) >>= case _ of
    -- If the issue body is not just a JSON string, then we don't consider it
    -- to be an attempted operation and it is presumably just an issue on the
    -- registry repository.
    NotJson ->
      pure unit

    MalformedJson issue err -> runRegistryM (mkEnv packagesMetadata issue) do
      comment $ fold
        [ "The JSON input for this package update is malformed:"
        , Utils.newlines 2
        , "```" <> err <> "```"
        , Utils.newlines 2
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
    Left err ->
      NotJson
    Right json -> case Json.decodeJson json of
      Left err -> MalformedJson issueNumber (Json.printJsonDecodeError err)
      Right op -> DecodedOperation issueNumber op

runOperation :: Operation -> RegistryM Unit
runOperation operation = case operation of
  Addition { packageName, fromBower, newRef, newPackageLocation, addToPackageSet } -> do
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

metadataDir :: String
metadataDir = "../metadata"

metadataFile :: PackageName -> String
metadataFile packageName = metadataDir <> "/" <> PackageName.print packageName <> ".json"

addOrUpdate :: { fromBower :: Boolean, ref :: String, packageName :: PackageName } -> Metadata -> RegistryM Unit
addOrUpdate { ref, fromBower, packageName } metadata = do
  -- let's get a temp folder to do our stuffs
  tmpDir <- liftEffect $ Tmp.mkTmpDir
  -- fetch the repo and put it in the tempdir, returning the name of its toplevel dir
  folderName <- case metadata.location of
    Git _ -> do
      -- TODO: remember subdir whenever we implement this
      throwWithComment "Packages are only allowed to come from GitHub for now. See #15"
    GitHub { owner, repo, subdir } -> do
      -- Check: subdir should not be there
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
  when fromBower $ do
    liftAff (Bower.readBowerfile (absoluteFolderPath <> "/bower.json")) >>= case _ of
      Left err -> throwWithComment $ "Error while reading Bowerfile: " <> err
      Right bowerfile -> do
        let manifestStr
              = Json.stringifyWithIndent 2 $ Json.encodeJson
              $ Bower.toManifest bowerfile ref metadata.location
        liftAff $ FS.writeTextFile UTF8 manifestPath manifestStr

  -- Try to read the manifest, typechecking it
  manifestExists <- liftAff $ FS.exists manifestPath
  manifest :: Manifest <- if (not manifestExists)
    then
      throwWithComment $ "Manifest not found at " <> manifestPath
    else do
      manifestStr <- liftAff $ FS.readTextFile UTF8 manifestPath
      liftAff (Dhall.jsonToDhallManifest manifestStr) >>= case _ of
        Left err ->
          throwWithComment $ "Could not type-check Manifest file: " <> err
        Right _ -> case fromJson manifestStr of
          Left err -> throwWithComment $ "Could not convert Manifest to JSON: " <> err
          Right res -> pure res

  -- TODO: pull the maintainers list from the manifest into the metadata?

  -- We need the version number to upload the package
  let newVersion = manifest.version

  runChecks metadata manifest

  -- After we pass all the checks it's time to do side effects and register the package
  log "Packaging the tarball to upload..."
  let newDirname = PackageName.print packageName <> "-" <> Version.print newVersion
  liftAff $ FS.rename absoluteFolderPath (tmpDir <> "/" <> newDirname)
  let tarballPath = tmpDir <> "/" <> newDirname <> ".tar.gz"
  liftEffect $ Tar.create { cwd: tmpDir, folderName: newDirname, archiveName: tarballPath }
  log "Hashing the tarball..."
  hash <- sha256sum tarballPath
  log $ "Hash: " <> hash
  log "Uploading package to the storage backend..."
  let uploadPackageInfo = { name: packageName, version: newVersion, revision: 0 }
  liftEffect $ PackageUpload.upload uploadPackageInfo tarballPath
  -- TODO: handle addToPackageSet
  log "Adding the new version to the package metadata file (hashes, etc)"
  let newMetadata = addVersionToMetadata newVersion { hash, ref } metadata
  liftAff $ FS.writeTextFile UTF8 (metadataFile packageName) (Json.stringifyWithIndent 2 $ Json.encodeJson newMetadata)
  -- FIXME: commit metadata file to master
  -- FIXME: update metadata in env or return it
  -- TODO: upload docs to pursuit


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
  Assert.shouldEqual libTarget.sources ["src/**/*.purs"]

  log "Check that version is unique"
  -- FIXME: lookup in the versions map, once we know the shape of it, see #80

  log "Check that all dependencies are contained in the registry"
  -- FIXME nice error message for this
  packages <- readPackagesMetadata
  let lookupPackage = flip Map.lookup packages <=< (hush <<< PackageName.parse)
  unless (all (isJust <<< lookupPackage) libTarget.dependencies) do
    pure unit

fromJson :: forall a. Json.DecodeJson a => String -> Either String a
fromJson = Json.jsonParser >=> (lmap Json.printJsonDecodeError <<< Json.decodeJson)

readJsonFile :: forall a. Json.DecodeJson a => String -> Aff a
readJsonFile path = do
  strResult <- FS.readTextFile UTF8 path
  case fromJson strResult of
    Left err -> Aff.throwError $ Aff.error $ "Error while parsing json from " <> path <> " : " <> err
    Right r -> pure r

mkNewMetadata :: Repo -> Metadata
mkNewMetadata location = { location, releases: mempty, unpublished: mempty, maintainers: mempty }

addVersionToMetadata :: Version -> Revision -> Metadata -> Metadata
addVersionToMetadata version revision metadata = do
  let version' = Version.print version
  metadata { releases = Object.insert version' [ revision ] metadata.releases }

sha256sum :: String -> RegistryM String
sha256sum filepath = do
  fileBuffer <- liftAff $ FS.readFile filepath
  liftEffect do
    newHash <- Hash.createHash Hash.SHA256
    fileHash <- Hash.update newHash fileBuffer
    digest <- Hash.digest fileHash
    Buffer.toString Hex digest

wget :: String -> String -> RegistryM Unit
wget url path = do
  let cmd = "wget"
  let stdin = Nothing
  let args = ["-O", path, url]
  result <- liftAff $ Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  case result.exit of
    NodeProcess.Normally 0 -> pure unit
    _ -> throwWithComment $ "Error while fetching tarball: " <> result.stderr
