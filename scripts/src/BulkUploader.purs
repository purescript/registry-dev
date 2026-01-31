-- | This script clears the S3 bucket and uploads all tarballs from local cache.
-- |
-- | WARNING: This is a destructive operation! It will delete ALL existing
-- | objects in the S3 bucket before uploading the local tarballs.
-- |
-- | Unlike the legacy importer or package deleter's --reimport flag, this script
-- | does NOT re-fetch sources, re-solve dependencies, or re-compile packages.
-- | It simply uploads what you already have locally.
-- |
-- | Use this after a successful legacy import run to sync your local state to
-- | the remote without risking spurious failures from re-running the publish
-- | pipeline.
-- |
-- | The script:
-- | 1. Clears the entire S3 bucket
-- | 2. Reads the manifest index in topological order
-- | 3. Uploads each tarball from local cache
-- |
-- | Example usage:
-- |
-- | ```sh
-- | nix run .#bulk-uploader -- dry-run  # See what would be uploaded
-- | nix run .#bulk-uploader -- upload   # Upload all tarballs
-- | ```
module Registry.Scripts.BulkUploader where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Control.Apply (lift2)
import Data.Array as Array
import Data.Formatter.DateTime as Formatter.DateTime
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry as Registry
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.S3 as S3
import Registry.Internal.Format as Internal.Format
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except as Except

data Mode = DryRun | Upload

derive instance Eq Mode

parser :: ArgParser Mode
parser = Arg.choose "command"
  [ Arg.flag [ "dry-run" ]
      "Log what would be uploaded without actually uploading."
      $> DryRun
  , Arg.flag [ "upload" ]
      "Upload all tarballs to S3."
      $> Upload
  ]

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv
  let description = "Upload local tarballs to S3 without re-publishing."
  mode <- case Arg.parseArgs "bulk-uploader" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit' 1)
    Right command -> pure command

  -- Environment
  _ <- Env.loadEnvFile ".env"
  resourceEnv <- Env.lookupResourceEnv
  s3Key <- lift2 { key: _, secret: _ } (Env.lookupRequired Env.spacesKey) (Env.lookupRequired Env.spacesSecret)

  -- Paths
  let cache = Path.concat [ scratchDir, ".cache" ]
  let registryPath = Path.concat [ scratchDir, "registry" ]
  let indexPath = Path.concat [ scratchDir, "registry-index" ]

  -- Logging
  now <- nowUTC
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  let logFile = "bulk-uploader-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]
  log $ "Logs available at " <> logPath

  runBulkUpload mode { s3Key, s3BucketUrl: resourceEnv.s3BucketUrl, cache, registryPath, indexPath }
    # Except.catch (\(msg :: String) -> Log.error msg *> Run.liftEffect (Process.exit' 1))
    # Log.interpret (\l -> Log.handleTerminal Normal l *> Log.handleFs Verbose logPath l)
    # Run.runBaseAff'

type BulkUploadEnv =
  { s3Key :: S3.SpaceKey
  , s3BucketUrl :: String
  , cache :: FilePath
  , registryPath :: FilePath
  , indexPath :: FilePath
  }

runBulkUpload :: forall r. Mode -> BulkUploadEnv -> Run (LOG + Except.EXCEPT String + AFF + EFFECT + r) Unit
runBulkUpload mode env = do
  -- Read the manifest index in toposorted order
  Log.info "Reading manifest index..."
  index <- Registry.readManifestIndexFromDisk env.indexPath
  let manifests = ManifestIndex.toSortedArray ManifestIndex.ConsiderRanges index
  let total = Array.length manifests
  Log.info $ "Found " <> show total <> " manifests in topological order"

  case mode of
    DryRun -> do
      Log.info "Dry run mode - would upload the following tarballs:"
      for_ manifests \(Manifest { name, version }) -> do
        let formatted = formatPackageVersion name version
        let tarballCacheKey = PackageName.print name <> "-" <> Version.print version
        let tarballPath = Path.concat [ env.cache, tarballCacheKey ]
        exists <- Run.liftAff (Aff.attempt (FS.Aff.stat tarballPath)) <#> isRight
        unless exists do
          Except.throw $ "Missing local tarball: " <> tarballPath
        Log.info $ "  " <> formatted
      Log.info ""
      Log.info "Run with 'upload' to actually upload."

    Upload -> do
      Log.info "Connecting to S3..."
      s3 <- connectS3 env.s3Key env.s3BucketUrl

      -- Step 1: Clear the entire bucket
      Log.info "Clearing existing bucket contents..."
      clearBucket s3

      -- Step 2: Upload all tarballs
      Log.info "Uploading tarballs..."

      forWithIndex_ manifests \idx (Manifest { name, version }) -> do
        let formatted = formatPackageVersion name version
        let progress = "[" <> show (idx + 1) <> "/" <> show total <> "] "
        let tarballCacheKey = PackageName.print name <> "-" <> Version.print version
        let tarballPath = Path.concat [ env.cache, tarballCacheKey ]
        let s3Path = PackageName.print name <> "/" <> Version.print version <> ".tar.gz"

        -- Check local tarball exists
        exists <- Run.liftAff (Aff.attempt (FS.Aff.stat tarballPath)) <#> isRight
        unless exists do
          Except.throw $ "Missing local tarball: " <> tarballPath

        Log.info $ progress <> "Uploading " <> formatted
        buffer <- Run.liftAff $ FS.Aff.readFile tarballPath
        uploadToS3 s3 s3Path buffer

      Log.info ""
      Log.info "Upload complete. To finish sync, push the registry repos manually:"
      Log.info $ "  cd " <> env.registryPath <> " && git push"
      Log.info $ "  cd " <> env.indexPath <> " && git push"

-- | Connect to the S3 bucket with retry
connectS3 :: forall r. S3.SpaceKey -> String -> Run (LOG + Except.EXCEPT String + AFF + r) S3.Space
connectS3 key space = do
  let bucket = "purescript-registry"
  Log.debug $ "Connecting to bucket " <> bucket <> " at " <> space
  Run.liftAff (withRetryOnTimeout (Aff.attempt (S3.connect key space bucket))) >>= case _ of
    Cancelled -> do
      Log.error "Timed out connecting to S3"
      Except.throw "Could not connect to S3"
    Failed err -> do
      Log.error $ "Failed to connect to S3: " <> Aff.message err
      Except.throw "Could not connect to S3"
    Succeeded conn -> do
      Log.debug "Connected to S3"
      pure conn

-- | Clear all objects from the S3 bucket
clearBucket :: forall r. S3.Space -> Run (LOG + Except.EXCEPT String + AFF + r) Unit
clearBucket s3 = do
  Log.info "Deleting all objects in bucket..."
  result <- Run.liftAff $ Aff.attempt $ S3.deleteAllObjects s3
  case result of
    Left err -> Except.throw $ "Failed to clear bucket: " <> Aff.message err
    Right count ->
      if count == 0 then
        Log.info "Bucket was already empty"
      else
        Log.info $ "Deleted " <> show count <> " objects from bucket"

-- | Upload a buffer to S3 with retry
uploadToS3 :: forall r. S3.Space -> String -> Buffer -> Run (LOG + Except.EXCEPT String + AFF + r) Unit
uploadToS3 s3 s3Path buffer = do
  Run.liftAff (withRetryOnTimeout (Aff.attempt (S3.putObject s3 { key: s3Path, body: buffer, acl: S3.PublicRead }))) >>= case _ of
    Cancelled -> do
      Log.error $ "Timed out uploading " <> s3Path
      Except.throw $ "Could not upload " <> s3Path
    Failed err -> do
      Log.error $ "Failed to upload " <> s3Path <> ": " <> Aff.message err
      Except.throw $ "Could not upload " <> s3Path
    Succeeded _ -> pure unit
