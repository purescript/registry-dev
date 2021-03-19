module Registry.API where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Argonaut as Json
import Data.Array (fold, replicate)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep as Generic
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodeUnits as String
import Dhall as Dhall
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
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

newtype ApiM a = ApiM (ReaderT Env Aff a)

derive instance newtypeApiM :: Newtype (ApiM a) _

derive newtype instance functorApiM :: Functor ApiM
derive newtype instance applyApiM :: Apply ApiM
derive newtype instance applicativeApiM :: Applicative ApiM
derive newtype instance bindApiM :: Bind ApiM
derive newtype instance monadApiM :: Monad ApiM
derive newtype instance monadEffectApiM :: MonadEffect ApiM
derive newtype instance monadAffApiM :: MonadAff ApiM
derive newtype instance monadErrorApiM :: MonadThrow Error ApiM
derive newtype instance monadAskApiM :: MonadAsk Env ApiM

runApiM :: forall a. Env -> ApiM a -> Aff a
runApiM env (ApiM m) = runReaderT m env

type Env =
  { comment :: String -> Aff Unit
  , commitToTrunk :: Aff Unit
  , uploadPackage :: Aff Unit
  }

mkEnv :: IssueNumber -> Env
mkEnv issue =
  { comment: void <<< GitHub.createComment issue
  , commitToTrunk: pure unit -- TODO
  , uploadPackage: pure unit -- TODO
  }

main :: Effect Unit
main = launchAff_ $ do
  eventPath <- liftEffect $ Env.lookupEnv "GITHUB_EVENT_PATH"
  readOperation (unsafePartial fromJust eventPath) >>= case _ of
    -- If the issue body is not just a JSON string, then we don't consider it
    -- to be an attempted operation and it is presumably just an issue on the
    -- registry repository.
    NotJson ->
      pure unit

    MalformedJson issue err -> runApiM (mkEnv issue) do
      comment $ fold
        [ "The JSON input for this package update is malformed:"
        , newlines 2
        , "```" <> err <> "```"
        , newlines 2
        , "You can try again by commenting on this issue with a corrected payload."
        ]

    DecodedOperation issue op ->
      runApiM (mkEnv issue) (runOperation op)

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
      throw $ "Error while parsing json from " <> eventPath <> " : " <> err
    Right event ->
      pure event

  pure $ case Json.jsonParser body of
    Left err ->
      NotJson
    Right json -> case Json.decodeJson json of
      Left err -> MalformedJson issueNumber (Json.printJsonDecodeError err)
      Right op -> DecodedOperation issueNumber op

runOperation :: Operation -> ApiM Unit
runOperation operation = ensureMetadataFolder *> case operation of
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
        metadata <- readJsonFile $ metadataFile packageName
        addOrUpdate { packageName, fromBower, ref: updateRef } metadata
      (throwWithComment "Metadata file should exist. Did you mean to create an Addition?")

  Unpublish _ -> throwWithComment "Unpublish not implemented! Ask us for help!" -- TODO


metadataDir :: String
metadataDir = "../metadata"

metadataFile :: String -> String
metadataFile packageName = metadataDir <> "/" <> packageName <> ".json"

ensureMetadataFolder :: ApiM Unit
ensureMetadataFolder = liftAff $ whenM (not <$> FS.exists metadataDir) $ FS.mkdir metadataDir


addOrUpdate :: { fromBower :: Boolean, ref :: String, packageName :: String } -> Metadata -> ApiM Unit
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
  let newDirname = packageName <> "-" <> newVersion
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
  -- TODO: publish github comments
  -- TODO: upload docs to pursuit


runChecks :: Metadata -> Manifest -> ApiM Unit
runChecks metadata manifest = do

  log "Checking that the Manifest includes the `lib` target"
  libTarget <- case Object.lookup "lib" manifest.targets of
    Nothing -> throwWithComment "Didn't find `lib` target in the Manifest!"
    Just a -> pure a

  log "Checking that `lib` target only includes `src`"
  Assert.shouldEqual libTarget.sources ["src/**/*.purs"]

  log "Checking that the package name fits the requirements"
  case parsePackageName manifest.name of
    Left err -> throwWithComment $ "Package name doesn't fit the requirements: " <> show err
    Right a -> pure unit

  -- For these we need to read all the metadatas in a hashmap:
  -- - FIXME: check that all dependencies are selfcontained in the registry
  -- - FIXME: version is unique!!
  -- - FIXME: package is unique

  unless (isValidSPDXLicenseId manifest.license) do
    throwWithComment $ "Invalid SPDX license: " <> manifest.license

fromJson :: forall a. Json.DecodeJson a => String -> Either String a
fromJson = Json.jsonParser >=> (lmap Json.printJsonDecodeError <<< Json.decodeJson)

readJsonFile :: forall a. Json.DecodeJson a => String -> ApiM a
readJsonFile path = do
  strResult <- liftAff $ FS.readTextFile UTF8 path
  case fromJson strResult of
    Left err -> throwWithComment $ "Error while parsing json from " <> path <> " : " <> err
    Right r -> pure r

mkNewMetadata :: Repo -> Metadata
mkNewMetadata location = { location, releases: mempty, unpublished: mempty, maintainers: mempty }

addVersionToMetadata :: String -> Revision -> Metadata -> Metadata
addVersionToMetadata version revision metadata = metadata { releases = Object.insert version [revision] metadata.releases }

throw :: forall m a. MonadThrow Error m => String -> m a
throw = Aff.throwError <<< Aff.error

comment :: String -> ApiM Unit
comment body = do
  postComment <- asks _.comment
  liftAff $ postComment body

-- | Throw an exception after commenting on the user's issue with the error
throwWithComment :: forall a. String -> ApiM a
throwWithComment body = do
  comment body
  throw body

sha256sum :: String -> ApiM String
sha256sum filepath = do
  fileBuffer <- liftAff $ FS.readFile filepath
  liftEffect do
    newHash <- Hash.createHash Hash.SHA256
    fileHash <- Hash.update newHash fileBuffer
    digest <- Hash.digest fileHash
    Buffer.toString Hex digest

wget :: String -> String -> ApiM Unit
wget url path = do
  let cmd = "wget"
  let stdin = Nothing
  let args = ["-O", path, url]
  result <- liftAff $ Process.spawn { cmd, stdin, args } NodeProcess.defaultSpawnOptions
  case result.exit of
    NodeProcess.Normally 0 -> pure unit
    _ -> throwWithComment $ "Error while fetching tarball: " <> result.stderr


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
