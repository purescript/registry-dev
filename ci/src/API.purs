module Registry.API where

import Prelude

import Data.Argonaut as Json
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
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
import Registry.Schema (Operation(..), Repo(..), Revision, Metadata)
import Sunde as Process
import Tmp as Tmp

type IssueNumber = String

type SideEffects =
  { commentOnIssue :: IssueNumber -> Aff Unit
  , commitToTrunk :: Aff Unit
  , uploadPackage :: Aff Unit
  }

-- main :: Effect Unit
-- main = parseOperation >=> runOperation


{-
TODO:
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
        -- fetch tarball from GitHub
        -- TODO support subdir
        tarballPath <- case newPackageLocation of
          Git _ -> throw "TODO: git clone not implemented"
          GitHub { owner, repo, subdir } -> do
            let tarballName = newRef <> ".tar.gz"
            let path = tmpDir <> "/" <> tarballName
            let archiveUrl = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> tarballName
            wget archiveUrl path
            pure path
        when fromBower do
          pure unit
          -- TODO: pull Bowerfile from the tarball, generate manifest from Bowerfile
          -- TODO: put manifest inside the tarball again
        -- TODO: run checks!!
        log "Hashing the tarball.."
        hash <- sha256sum tarballPath
        -- upload package to the storage backend
        -- TODO get version from manifest
        let newVersion = "bogus"
        let uploadPackageInfo = { name: packageName, version: newVersion, revision: 0 }
        liftEffect $ PackageUpload.upload uploadPackageInfo tarballPath
        -- TODO: handle addToPackageSet
        log "Adding the new version to the package metadata file (hashes, etc)"
        let newMetadata = addVersionToMetadata newVersion { hash, ref: newRef } $ mkNewMetadata newPackageLocation
        FS.writeTextFile UTF8 packageMetadataFile (stringifyWithIndent 2 $ Json.encodeJson newMetadata)
        -- TODO: publish github comments
  -- TODO: implement more operations
  other -> throw $ "Unsupported operation: " <> show other

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
