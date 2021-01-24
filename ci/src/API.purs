module Registry.API where

import Prelude

import Data.Argonaut as Json
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Dhall as Dhall
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log)
import Foreign.Object as Object
import Node.Buffer as Buffer
import Node.ChildProcess as NodeProcess
import Node.Crypto.Hash as Hash
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import PackageUpload as PackageUpload
import Registry.BowerImport as Bower
import Registry.Schema (Manifest, Metadata, Operation(..), Repo(..), Revision)
import Sunde as Process
import Tar as Tar
import Test.Spec.Assertions as Assert
import Tmp as Tmp

type IssueNumber = String

type SideEffects =
  { commentOnIssue :: IssueNumber -> Aff Unit
  , commitToTrunk :: Aff Unit
  , uploadPackage :: Aff Unit
  }

main :: Effect Unit
main = launchAff_ $ runOperation $
  Addition
    { packageName: "aff"
    , fromBower: true
    , newRef: "v5.1.2"
    , newPackageLocation: GitHub { subdir: Nothing, owner: "purescript-contrib", repo: "purescript-aff"}
    , addToPackageSet: false
    }


-- parseOperation >=> runOperation


{-
FIXME:
- get json from issue/issue_comment, get body out of there
- tests for that
-}

runOperation :: Operation -> Aff Unit
runOperation = case _ of
  Addition { packageName, fromBower, newRef, newPackageLocation, addToPackageSet } -> do
    -- check that we don't have a metadata file for that package
    -- (first check that the metadata folder exists)
    let metadataDir = "../metadata"
    let packageMetadataFile = metadataDir <> "/" <> packageName <> ".json"
    whenM (not <$> FS.exists metadataDir) $ FS.mkdir metadataDir
    ifM (FS.exists packageMetadataFile)
      -- if the metadata file already exists then we steer this to be an Update instead
      (runOperation $ Update { packageName, fromBower, updateRef: newRef })
      do
        -- let's get a temp folder to do our stuffs
        tmpDir <- liftEffect $ Tmp.mkTmpDir

        -- fetch the repo and put it in the tempdir, returning the name of its toplevel dir
        folderName <- case newPackageLocation of
          Git _ -> do
            -- TODO: remember subdir
            throw "TODO: git clone not implemented"
          GitHub { owner, repo, subdir } -> do
            -- Check: subdir should not be there
            when (isJust subdir) $ throw "`subdir` is not supported for now. See #16"

            let tarballName = newRef <> ".tar.gz"
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
              -- FIXME: check that all dependencies are selfcontained in the registry
              let manifestStr
                    = stringifyWithIndent 2 $ Json.encodeJson
                    $ Bower.toManifest bowerfile newRef newPackageLocation
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

        -- We need the version number to upload the package
        let newVersion = manifest.version

        -- Checks:
        log "Checking that the Manifest includes the `lib` target"
        libTarget <- case Object.lookup "lib" manifest.targets of
          Nothing -> throw "Didn't find `lib` target in the Manifest!"
          Just a -> pure a
        log "Checking that `lib` target only includes `src`"
        Assert.shouldEqual libTarget.sources ["src/**/*.purs"]
        -- TODO: run the other checks!!

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
        let newMetadata = addVersionToMetadata newVersion { hash, ref: newRef } $ mkNewMetadata newPackageLocation
        FS.writeTextFile UTF8 packageMetadataFile (stringifyWithIndent 2 $ Json.encodeJson newMetadata)
        -- FIXME: commit metadata file to master
        -- TODO: publish github comments
  -- FIXME: implement more operations
  other -> throw $ "Unsupported operation: " <> show other

fromJson :: forall a. Json.DecodeJson a => String -> Either String a
fromJson = Json.jsonParser >=> (lmap Json.printJsonDecodeError <<< Json.decodeJson)

-- TODO: tests for parsing operation
parseOperation :: String -> Aff Operation
parseOperation operationStr = do
  case (Json.jsonParser operationStr >>= (lmap Json.printJsonDecodeError <<< Json.decodeJson)) of
    Left err -> do
      error $ "Got error while parsing Operation"
      Aff.throwError $ Aff.error err
    Right (op :: Operation) -> pure op

mkNewMetadata :: Repo -> Metadata
mkNewMetadata location = { location, releases: mempty, unpublished: mempty, maintainers: mempty }

addVersionToMetadata :: String -> Revision -> Metadata -> Metadata
addVersionToMetadata version revision metadata = metadata { releases = Object.insert version [revision] metadata.releases }

-- TODO: we want to leave a GitHub comment before killing the process here
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


