module Registry.API where

import Prelude

import Data.Argonaut as Json
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Array (fold, replicate)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep as Generic
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodeUnits as String
import Dhall as Dhall
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
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
import Registry.BowerImport (stripPurescriptPrefix)
import Registry.BowerImport as Bower
import Registry.SPDX (isValidSPDXLicenseId)
import Registry.Schema (Manifest, Metadata, Operation(..), Repo(..), Revision)
import Sunde as Process
import Tar as Tar
import Test.Spec.Assertions as Assert
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodePoints as Parse
import Text.Parsing.StringParser.Combinators ((<?>))
import Text.Parsing.StringParser.Combinators as ParseC
import Tmp as Tmp

type SideEffects =
  { commentOnIssue :: IssueNumber -> Aff Unit
  , commitToTrunk :: Aff Unit
  , uploadPackage :: Aff Unit
  }

main :: Effect Unit
main = launchAff_ $ do
  eventPath <- liftEffect $ Env.lookupEnv "GITHUB_EVENT_PATH"
  readOperation (unsafePartial fromJust eventPath) >>= case _ of
    NotJson ->
      -- If the issue body is not just a JSON string, then we don't consider it
      -- to be an attempted operation and it is presumably just an issue on the
      -- registry repository.
      pure unit

    MalformedJson issue err -> do
      let
        commentBody = fold
          [ "The JSON input for this package update is malformed:"
          , newlines 2
          , "```" <> err <> "```"
          , newlines 2
          , "You can try again by commenting on this issue with a corrected payload."
          ]

      commentId <- GitHub.createComment issue commentBody
      pure unit

    DecodedOperation issue op ->
      runOperation op

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
  GitHub.Event { issueNumber, body } <- readJsonFile eventPath
  pure $ case Json.jsonParser body of
    Left err ->
      NotJson
    Right json -> case Json.decodeJson json of
      Left err -> MalformedJson issueNumber (Json.printJsonDecodeError err)
      Right op -> DecodedOperation issueNumber op

runOperation :: Operation -> Aff Unit
runOperation operation = ensureMetadataFolder *> case operation of
  Addition { packageName, fromBower, newRef, newPackageLocation, addToPackageSet } -> do
    -- check that we don't have a metadata file for that package
    ifM (FS.exists $ metadataFile packageName)
      -- if the metadata file already exists then we steer this to be an Update instead
      (runOperation $ Update { packageName, fromBower, updateRef: newRef })
      do
        addOrUpdate { packageName, fromBower, ref: newRef } $ mkNewMetadata newPackageLocation

  Update { packageName, fromBower, updateRef } -> do
    ifM (FS.exists $ metadataFile packageName)
      do
        metadata <- readJsonFile $ metadataFile packageName
        addOrUpdate { packageName, fromBower, ref: updateRef } metadata
      (throw "Metadata file should exist. Did you mean to create an Addition?")

  Unpublish _ -> throw "Unpublish not implemented!" -- TODO


metadataDir :: String
metadataDir = "../metadata"

metadataFile :: String -> String
metadataFile packageName = metadataDir <> "/" <> packageName <> ".json"

ensureMetadataFolder :: Aff Unit
ensureMetadataFolder = whenM (not <$> FS.exists metadataDir) $ FS.mkdir metadataDir


addOrUpdate :: { fromBower :: Boolean, ref :: String, packageName :: String } -> Metadata -> Aff Unit
addOrUpdate { ref, fromBower, packageName } metadata = do
  -- let's get a temp folder to do our stuffs
  tmpDir <- liftEffect $ Tmp.mkTmpDir
  -- fetch the repo and put it in the tempdir, returning the name of its toplevel dir
  folderName <- case metadata.location of
    Git _ -> do
      -- TODO: remember subdir whenever we implement this
      throw "Packages are only allowed to come from GitHub for now. See #15"
    GitHub { owner, repo, subdir } -> do
      -- Check: subdir should not be there
      when (isJust subdir) $ throw "`subdir` is not supported for now. See #16"

      let tarballName = ref <> ".tar.gz"
      let absoluteTarballPath = tmpDir <> "/" <> tarballName
      let archiveUrl = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> tarballName
      log $ "Fetching tarball from GitHub: " <> archiveUrl
      wget archiveUrl absoluteTarballPath
      log $ "Tarball downloaded in " <> absoluteTarballPath
      liftEffect (Tar.getToplevelDir absoluteTarballPath) >>= case _ of
        Nothing -> throw "Could not find a toplevel dir in the tarball!"
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
    Bower.readBowerfile (absoluteFolderPath <> "/bower.json") >>= case _ of
      Left err -> throw $ "Error while reading Bowerfile: " <> err
      Right bowerfile -> do
        let manifestStr
              = stringifyWithIndent 2 $ Json.encodeJson
              $ Bower.toManifest bowerfile ref metadata.location
        FS.writeTextFile UTF8 manifestPath manifestStr

  -- Try to read the manifest, typechecking it
  manifest :: Manifest <- ifM (not <$> FS.exists manifestPath)
    (throw $ "Manifest not found at " <> manifestPath)
    do
      manifestStr <- FS.readTextFile UTF8 manifestPath
      Dhall.jsonToDhall manifestStr >>= case _ of
        Left err -> throw err
        Right _ -> case fromJson manifestStr of
          Left err -> throw err
          Right res -> pure res

  -- TODO: pull the maintainers list from the manifest into the metadata?

  -- We need the version number to upload the package
  let newVersion = manifest.version

  runChecks metadata manifest

  -- After we pass all the checks it's time to do side effects and register the package
  log "Packaging the tarball to upload..."
  let newDirname = packageName <> "-" <> newVersion
  FS.rename absoluteFolderPath (tmpDir <> "/" <> newDirname)
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
  FS.writeTextFile UTF8 (metadataFile packageName) (stringifyWithIndent 2 $ Json.encodeJson newMetadata)
  -- FIXME: commit metadata file to master
  -- TODO: publish github comments
  -- TODO: upload docs to pursuit


runChecks :: Metadata -> Manifest -> Aff Unit
runChecks metadata manifest = do

  log "Checking that the Manifest includes the `lib` target"
  libTarget <- case Object.lookup "lib" manifest.targets of
    Nothing -> throw "Didn't find `lib` target in the Manifest!"
    Just a -> pure a

  log "Checking that `lib` target only includes `src`"
  Assert.shouldEqual libTarget.sources ["src/**/*.purs"]

  log "Checking that the package name fits the requirements"
  case parsePackageName manifest.name of
    Right a -> pure unit
    Left err -> throw $ show err

  -- For these we need to read all the metadatas in a hashmap:
  -- - FIXME: check that all dependencies are selfcontained in the registry
  -- - FIXME: version is unique!!
  -- - FIXME: package is unique

  if isValidSPDXLicenseId manifest.license then
    pure unit
  else
    throw $ "Invalid SPDX license: " <> manifest.license

fromJson :: forall a. Json.DecodeJson a => String -> Either String a
fromJson = Json.jsonParser >=> (lmap Json.printJsonDecodeError <<< Json.decodeJson)

readJsonFile :: forall a. Json.DecodeJson a => String -> Aff a
readJsonFile path = do
  strResult <- FS.readTextFile UTF8 path
  case fromJson strResult of
    Left err -> throw $ "Error while parsing json from " <> path <> " : " <> err
    Right r -> pure r

mkNewMetadata :: Repo -> Metadata
mkNewMetadata location = { location, releases: mempty, unpublished: mempty, maintainers: mempty }

addVersionToMetadata :: String -> Revision -> Metadata -> Metadata
addVersionToMetadata version revision metadata = metadata { releases = Object.insert version [revision] metadata.releases }

-- FIXME: we want to leave a GitHub comment before killing the process here
throw :: forall a. String -> Aff a
throw = Aff.throwError <<< Aff.error

sha256sum :: String -> Aff String
sha256sum filepath = do
  fileBuffer <- FS.readFile filepath
  liftEffect do
    newHash <- Hash.createHash Hash.SHA256
    fileHash <- Hash.update newHash fileBuffer
    digest <- Hash.digest fileHash
    Buffer.toString Hex digest

wget :: String -> String -> Aff Unit
wget url path = do
  let cmd = "wget"
  let stdin = Nothing
  let args = ["-O", path, url]
  result <- Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  case result.exit of
    NodeProcess.Normally 0 -> pure unit
    _ -> throw $ "Error while fetching tarball: " <> result.stderr


-- TODO: move this to a smart constructor for PackageName?
parsePackageName :: String -> Either Parser.ParseError String
parsePackageName = Parser.runParser do
  -- Error messages, which also define our rules for package names
  let endErr = "Package name should end with a lower case char or digit"
  let charErr = "Package name can contain lower case chars, digits and non-consecutive dashes"
  let startErr = "Package name should start with a lower case char or a digit"
  let manyDashesErr = "Package names cannot contain consecutive dashes"

  let char = ParseC.choice [Parse.lowerCaseChar, Parse.anyDigit] <?> charErr
  let dash = void $ Parse.char '-'
  let chunk = ParseC.many1 char

  -- A "chunk" is an alphanumeric word between dashes
  firstChunk <- chunk <?> startErr
  nextChunks <- do
    chunks <- ParseC.many do
      void dash
      void $ ParseC.optionMaybe (ParseC.lookAhead Parse.anyChar) >>= case _ of
        Just '-' -> Parser.fail manyDashesErr
        Just _ -> pure unit
        Nothing -> ParseC.lookAhead Parse.eof *> Parser.fail endErr
      map (NEL.cons '-') chunk <?> endErr
    pure chunks
  -- Make sure that we consume all the string in input
  Parse.eof <?> charErr

  -- put together the string, stripping the "purescript-" prefix if there
  let chars = List.concat $ map NEL.toList $ List.Cons firstChunk nextChunks
  let name = stripPurescriptPrefix $ fromCharArray $ List.toUnfoldable $ chars
  -- and check that it's not longer than 50 chars
  if String.length name > 50
  then Parser.fail "Package name cannot be longer than 50 chars"
  else pure name

newlines :: Int -> String
newlines n = fold $ replicate n "\n"
