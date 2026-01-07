-- | Shared environment and helper functions for E2E tests.
-- |
-- | This module centralizes common operations needed across E2E test suites:
-- | - Environment configuration from env vars
-- | - WireMock reset helpers for test isolation
-- | - Job polling with automatic failure handling
-- |
-- | Use these helpers instead of defining local versions in each test module.
module Test.E2E.Support.Env
  ( getConfig
  , getStateDir
  , getPrivateKey
  , resetTestState
  , resetStorageMock
  , clearGithubRequests
  , clearStorageRequests
  , getStorageRequests
  , resetDatabase
  , resetGitFixtures
  , resetLogs
  , resetCache
  , pollJobOrFail
  , pollJobExpectFailure
  , expectRight
  , signUnpublishOrFail
  , signTransferOrFail
  , gitStatus
  , isCleanGitStatus
  , waitForAllMatrixJobs
  , waitForMatrixJobStart
  , isMatrixJobFor
  , readMetadata
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String as String
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.ChildProcess.Types (Exit(..))
import Node.Library.Execa as Execa
import Node.Path as Path
import Registry.API.V1 (Job(..))
import Registry.API.V1 as V1
import Registry.App.Effect.Env as Env
import Registry.App.Prelude (readJsonFile)
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Metadata (Metadata)
import Registry.Metadata as Metadata
import Registry.Operation (AuthenticatedData, TransferData, UnpublishData)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Test.Assert as Assert
import Registry.Version (Version)
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Fixtures as Fixtures
import Test.E2E.Support.WireMock as WireMock

-- | Get client config from environment.
-- | Reads SERVER_PORT to construct the base URL.
getConfig :: Aff Client.Config
getConfig = liftEffect Client.configFromEnv

-- | Get the pacchettibotti private key from environment (base64-decoded).
-- | Used for signing authenticated operations in E2E tests.
getPrivateKey :: Aff String
getPrivateKey = liftEffect $ Env.lookupRequired Env.pacchettibottiED25519

-- | Reset all test state for isolation between tests.
-- | This is the recommended way to set up test isolation in Spec.before_.
-- | Resets: database, git fixtures, storage mock, and logs.
resetTestState :: Aff Unit
resetTestState = do
  resetDatabase
  resetGitFixtures
  resetStorageMock
  resetLogs

-- | Reset the unified storage WireMock instance (S3, bucket, Pursuit).
-- | Clears request journal and resets all scenarios to "Started" state.
resetStorageMock :: Aff Unit
resetStorageMock = do
  storageConfig <- liftEffect WireMock.configForStorage
  WireMock.clearRequestsOrFail storageConfig
  WireMock.resetScenariosOrFail storageConfig

-- | Clear the GitHub WireMock request journal.
-- | Use this when you need to inspect GitHub API requests in isolation.
clearGithubRequests :: Aff Unit
clearGithubRequests = do
  wmConfig <- liftEffect WireMock.configFromEnv
  WireMock.clearRequestsOrFail wmConfig

-- | Clear the storage WireMock request journal (S3, Pursuit).
-- | Use this when you need to inspect storage API requests in isolation.
clearStorageRequests :: Aff Unit
clearStorageRequests = do
  storageConfig <- liftEffect WireMock.configForStorage
  WireMock.clearRequestsOrFail storageConfig

-- | Get captured requests from the storage WireMock (S3, Pursuit).
-- | Use this to verify S3 uploads/deletes or Pursuit API calls.
getStorageRequests :: Aff (Array WireMock.WireMockRequest)
getStorageRequests = do
  storageConfig <- liftEffect WireMock.configForStorage
  WireMock.getRequestsOrFail storageConfig

-- | Reset the database by clearing all job-related tables.
-- |
-- | This works because all job tables (publish_jobs, unpublish_jobs, transfer_jobs,
-- | matrix_jobs, package_set_jobs, logs) have foreign keys to job_info with
-- | ON DELETE CASCADE. See db/schema.sql for the schema definition.
-- |
-- | If new tables are added that don't cascade from job_info, they must be
-- | explicitly deleted here.
resetDatabase :: Aff Unit
resetDatabase = do
  stateDirPath <- getStateDir
  let dbPath = Path.concat [ stateDirPath, "db", "registry.sqlite3" ]
  result <- _.getResult =<< Execa.execa "sqlite3" [ dbPath, "DELETE FROM job_info;" ] identity
  case result.exit of
    Normally 0 -> pure unit
    _ -> Aff.throwError $ Aff.error $ "Failed to reset database: " <> result.stderr

-- | Reset the git fixtures to restore original state.
-- | This restores metadata files modified by unpublish/transfer operations.
-- | 
-- | Strategy: Reset the origin repos to their initial-fixture tag (created during
-- | setup), then delete the server's scratch git clones. The server will
-- | re-clone fresh copies on the next operation, ensuring a clean cache state.
-- | Note: We only delete the git repo clones, not the entire scratch directory,
-- | to preserve the logs subdirectory which the server expects to exist.
resetGitFixtures :: Aff Unit
resetGitFixtures = do
  stateDirPath <- getStateDir
  fixturesDir <- liftEffect $ Env.lookupRequired Env.repoFixturesDir
  let
    registryOrigin = Path.concat [ fixturesDir, "purescript", "registry" ]
    registryIndexOrigin = Path.concat [ fixturesDir, "purescript", "registry-index" ]
    scratchDir = Path.concat [ stateDirPath, "scratch" ]
  resetOrigin registryOrigin
  resetOrigin registryIndexOrigin
  deleteGitClones scratchDir
  where
  resetOrigin dir = do
    resetResult <- _.getResult =<< Execa.execa "git" [ "reset", "--hard", "initial-fixture" ] (_ { cwd = Just dir })
    case resetResult.exit of
      Normally 0 -> pure unit
      _ -> Aff.throwError $ Aff.error $ "Failed to reset git repo " <> dir <> ": " <> resetResult.stderr
    cleanResult <- _.getResult =<< Execa.execa "git" [ "clean", "-fd" ] (_ { cwd = Just dir })
    case cleanResult.exit of
      Normally 0 -> pure unit
      _ -> Aff.throwError $ Aff.error $ "Failed to clean git repo " <> dir <> ": " <> cleanResult.stderr
  deleteGitClones scratchDir = do
    let registryClone = Path.concat [ scratchDir, "registry" ]
    let registryIndexClone = Path.concat [ scratchDir, "registry-index" ]
    deleteDir registryClone
    deleteDir registryIndexClone
  deleteDir dir = do
    result <- _.getResult =<< Execa.execa "rm" [ "-rf", dir ] identity
    case result.exit of
      Normally 0 -> pure unit
      _ -> Aff.throwError $ Aff.error $ "Failed to delete directory " <> dir <> ": " <> result.stderr

-- | Clear server log files for test isolation.
-- | Deletes *.log files from the scratch/logs directory but preserves the directory itself.
resetLogs :: Aff Unit
resetLogs = do
  stateDir <- getStateDir
  let logsDir = Path.concat [ stateDir, "scratch", "logs" ]
  -- Quote path to handle spaces or special characters safely
  let cmd = "rm -f '" <> logsDir <> "'/*.log 2>/dev/null || true"
  result <- _.getResult =<< Execa.execa "sh" [ "-c", cmd ] identity
  case result.exit of
    Normally _ -> pure unit
    _ -> pure unit

-- | Clear the scratch cache directory.
-- | This ensures each test makes fresh API calls rather than using cached responses.
-- | Important for tests that verify specific API calls were made (e.g., Teams API).
resetCache :: Aff Unit
resetCache = do
  stateDir <- getStateDir
  let cacheDir = Path.concat [ stateDir, "scratch", ".cache" ]
  FS.Extra.remove cacheDir

-- | Poll a job until completion, failing the test if the job fails.
-- | Prints error logs on failure for debugging.
-- |
-- | This is the recommended way to wait for job completion in E2E tests.
-- | Do not implement custom polling loops; use this helper instead.
pollJobOrFail :: Client.Config -> V1.JobId -> Aff V1.Job
pollJobOrFail config jobId = do
  job <- Client.pollJob config jobId
  unless (V1.jobInfo job).success do
    Console.log "Job failed! Logs:"
    let logMessages = map (\l -> "[" <> V1.printLogLevel l.level <> "] " <> l.message) (V1.jobInfo job).logs
    Console.log $ String.joinWith "\n" logMessages
    let errorLogs = Array.filter (\l -> l.level == V1.Error) (V1.jobInfo job).logs
    let errorMessages = map _.message errorLogs
    Assert.fail $ "Job failed with errors:\n" <> String.joinWith "\n" errorMessages
  pure job

-- | Poll a job until completion, expecting it to fail.
-- | Returns the job for further assertions on error messages.
-- | Fails the test if the job unexpectedly succeeds.
pollJobExpectFailure :: Client.Config -> V1.JobId -> Aff V1.Job
pollJobExpectFailure config jobId = do
  job <- Client.pollJob config jobId
  when (V1.jobInfo job).success do
    Assert.fail "Expected job to fail, but it succeeded"
  pure job

-- | Unwrap an Either, failing the test with a descriptive message on Left.
-- | Use this to reduce boilerplate when handling client responses.
-- |
-- | Example:
-- | ```purescript
-- | { jobId } <- expectRight "publish effect" =<< Client.publish config publishData
-- | ```
expectRight :: forall a. String -> Either Client.ClientError a -> Aff a
expectRight context = case _ of
  Left err -> Aff.throwError $ Aff.error $ context <> ": " <> Client.printClientError err
  Right a -> pure a

-- | Sign an unpublish operation using the pacchettibotti private key from environment.
-- | Fails the test if signing fails.
signUnpublishOrFail :: UnpublishData -> Aff AuthenticatedData
signUnpublishOrFail unpublishData = do
  privateKey <- getPrivateKey
  case Fixtures.signUnpublish privateKey unpublishData of
    Left err -> Aff.throwError $ Aff.error $ "Failed to sign unpublish: " <> err
    Right authData -> pure authData

-- | Sign a transfer operation using the pacchettibotti private key from environment.
-- | Fails the test if signing fails.
signTransferOrFail :: TransferData -> Aff AuthenticatedData
signTransferOrFail transferData = do
  privateKey <- getPrivateKey
  case Fixtures.signTransfer privateKey transferData of
    Left err -> Aff.throwError $ Aff.error $ "Failed to sign transfer: " <> err
    Right authData -> pure authData

-- | Get the STATE_DIR environment variable.
-- | This is the root directory for test state (database, scratch repos, etc).
getStateDir :: Aff String
getStateDir = liftEffect $ Env.lookupRequired Env.stateDir

-- | Run git status --porcelain in a directory and return the output.
-- | Empty output means clean working tree. Uses the git CLI directly.
gitStatus :: String -> Aff String
gitStatus cwd = do
  result <- _.getResult =<< Execa.execa "git" [ "status", "--porcelain" ] (_ { cwd = Just cwd })
  case result.exit of
    Normally 0 -> pure $ String.trim result.stdout
    _ -> Aff.throwError $ Aff.error $ "git status failed in " <> cwd <> ": " <> result.stderr

-- | Check if git status output indicates a clean working tree (no changes).
isCleanGitStatus :: String -> Boolean
isCleanGitStatus status = String.null status

-- | Wait for all matrix jobs for a package version to complete.
-- | Matrix jobs are created when a package is published to test it against
-- | multiple compiler versions. This helper polls until at least one matrix job
-- | exists and all have finished. Dynamically detects how many jobs were spawned.
waitForAllMatrixJobs :: Client.Config -> PackageName -> Version -> Aff Unit
waitForAllMatrixJobs config packageName packageVersion = go 120 0
  where
  go :: Int -> Int -> Aff Unit
  go 0 _ = Aff.throwError $ Aff.error "Timed out waiting for matrix jobs to complete"
  go attempts lastCount = do
    jobsResult <- Client.getJobs config
    case jobsResult of
      Left err -> Aff.throwError $ Aff.error $ "Failed to get jobs: " <> Client.printClientError err
      Right jobs -> do
        let
          matrixJobs = Array.filter (isMatrixJobFor packageName packageVersion) jobs
          totalCount = Array.length matrixJobs
          finishedCount = Array.length $ Array.filter (\j -> isJust (V1.jobInfo j).finishedAt) matrixJobs
          allFinished = finishedCount == totalCount
          -- Jobs are still being created if count increased since last poll
          stillCreating = totalCount > lastCount
        if totalCount >= 1 && allFinished && not stillCreating then
          pure unit
        else do
          when (attempts `mod` 10 == 0) do
            Console.log $ "Waiting for matrix jobs: " <> show finishedCount <> "/" <> show totalCount <> " finished"
          Aff.delay (Milliseconds 1000.0)
          go (attempts - 1) totalCount

-- | Wait until at least one matrix job has started (has startedAt but no finishedAt).
-- | Useful for testing job priority - submit a publish job while matrix jobs are running.
waitForMatrixJobStart :: Client.Config -> PackageName -> Version -> Aff Unit
waitForMatrixJobStart config packageName packageVersion = go 30
  where
  go :: Int -> Aff Unit
  go 0 = Aff.throwError $ Aff.error "Timed out waiting for matrix job to start"
  go attempts = do
    jobsResult <- Client.getJobs config
    case jobsResult of
      Left err -> Aff.throwError $ Aff.error $ "Failed to get jobs: " <> Client.printClientError err
      Right jobs -> do
        let
          matrixJobs = Array.filter (isMatrixJobFor packageName packageVersion) jobs
          startedNotFinished = Array.any
            (\j -> isJust (V1.jobInfo j).startedAt && isNothing (V1.jobInfo j).finishedAt)
            matrixJobs
        if startedNotFinished then
          pure unit
        else do
          when (attempts `mod` 10 == 0) do
            Console.log $ "Waiting for matrix job to start (" <> show (Array.length matrixJobs) <> " jobs found)"
          Aff.delay (Milliseconds 200.0)
          go (attempts - 1)

-- | Check if a job is a matrix job for the given package and version.
isMatrixJobFor :: PackageName -> Version -> Job -> Boolean
isMatrixJobFor expectedName expectedVersion = case _ of
  MatrixJob { packageName, packageVersion } ->
    packageName == expectedName && packageVersion == expectedVersion
  _ -> false

-- | Read and parse the metadata file for a package from the server's scratch clone.
-- | Returns a typed Metadata value.
-- | Use this to verify metadata changes after publish/unpublish/transfer operations.
readMetadata :: PackageName -> Aff Metadata
readMetadata packageName = do
  stateDir <- getStateDir
  let metadataPath = Path.concat [ stateDir, "scratch", "registry", "metadata", PackageName.print packageName <> ".json" ]
  readJsonFile Metadata.codec metadataPath >>= case _ of
    Left err -> Aff.throwError $ Aff.error $ "Failed to read metadata for " <> PackageName.print packageName <> ": " <> err
    Right metadata -> pure metadata
