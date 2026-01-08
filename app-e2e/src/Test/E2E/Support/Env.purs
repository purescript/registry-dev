-- | Shared environment and helper functions for E2E tests.
-- |
-- | This module provides:
-- | - TestEnv type and E2E monad for test helpers (re-exported from Types)
-- | - Environment construction from env vars (mkTestEnv)
-- | - WireMock reset helpers for test isolation
-- | - Job polling with automatic failure handling
-- | - Git and metadata state inspection
-- |
-- | All functions operate in the E2E monad (ReaderT TestEnv Aff), so they
-- | have access to the shared test environment without explicit passing.
module Test.E2E.Support.Env
  ( module ReExports
  , mkTestEnv
  , runE2E
  , resetTestState
  , resetDatabase
  , resetGitFixtures
  , resetLogs
  , resetGitHubRequestCache
  , pollJobOrFail
  , pollJobExpectFailure
  , signUnpublishOrFail
  , signTransferOrFail
  , gitStatus
  , isCleanGitStatus
  , waitForAllMatrixJobs
  , isMatrixJobFor
  , readMetadata
  , readManifestIndexEntry
  , manifestIndexEntryExists
  , assertReposClean
  , hasStorageUpload
  , hasStorageDelete
  ) where

import Registry.App.Prelude

import Control.Monad.Reader (ask, runReaderT)
import Data.Array as Array
import Data.String as String
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Node.ChildProcess.Types (Exit(..))
import Node.FS.Aff as FS.Aff
import Node.Library.Execa as Execa
import Node.Path as Path
import Registry.API.V1 (Job(..))
import Registry.API.V1 as V1
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Env as Env
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata (Metadata)
import Registry.Metadata as Metadata
import Registry.Operation (AuthenticatedData, TransferData, UnpublishData)
import Registry.PackageName as PackageName
import Registry.Test.Assert as Assert
import Registry.Version as Version
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Fixtures (PackageFixture)
import Test.E2E.Support.Fixtures as Fixtures
import Test.E2E.Support.Types (ClientConfig, E2E, E2ESpec, TestEnv, WireMockConfig) as ReExports
import Test.E2E.Support.Types (E2E, TestEnv)
import Test.E2E.Support.WireMock as WireMock

-- | Build the test environment from environment variables.
-- | Called once at startup in Main, before running any tests.
mkTestEnv :: Effect TestEnv
mkTestEnv = do
  port <- Env.lookupRequired Env.serverPort
  let
    clientConfig =
      { baseUrl: "http://localhost:" <> show port
      , pollInterval: Milliseconds 2000.0
      , maxPollAttempts: 30
      }

  githubUrl <- Env.lookupRequired Env.githubApiUrl
  storageUrl <- Env.lookupRequired Env.s3ApiUrl
  let
    githubWireMock = { baseUrl: githubUrl }
    storageWireMock = { baseUrl: storageUrl }

  stateDir <- Env.lookupRequired Env.stateDir
  privateKey <- Env.lookupRequired Env.pacchettibottiED25519

  pure { clientConfig, githubWireMock, storageWireMock, stateDir, privateKey }

-- | Run an E2E computation with a given environment.
-- | Primarily used by hoistSpec in Main.
runE2E :: forall a. TestEnv -> E2E a -> Aff a
runE2E env = flip runReaderT env

-- | Reset all test state for isolation between tests.
-- | This is the recommended way to set up test isolation in Spec.before_.
-- | Resets: database, git fixtures, storage mock, and logs.
resetTestState :: E2E Unit
resetTestState = do
  resetDatabase
  resetGitFixtures
  WireMock.clearStorageRequests
  WireMock.resetStorageScenarios
  WireMock.clearGithubRequests
  resetGitHubRequestCache
  resetLogs

-- | Reset the database by clearing all job-related tables.
-- |
-- | This works because all job tables (publish_jobs, unpublish_jobs, transfer_jobs,
-- | matrix_jobs, package_set_jobs, logs) have foreign keys to job_info with
-- | ON DELETE CASCADE. See db/schema.sql for the schema definition.
resetDatabase :: E2E Unit
resetDatabase = do
  { stateDir } <- ask
  let dbPath = Path.concat [ stateDir, "db", "registry.sqlite3" ]
  result <- liftAff $ _.getResult =<< Execa.execa "sqlite3" [ dbPath, "DELETE FROM job_info;" ] identity
  case result.exit of
    Normally 0 -> pure unit
    _ -> liftAff $ Aff.throwError $ Aff.error $ "Failed to reset database: " <> result.stderr

-- | Reset the git fixtures to restore original state.
-- | This restores metadata files modified by unpublish/transfer operations.
-- |
-- | Strategy: Reset the origin repos to their initial-fixture tag (created during
-- | setup), then delete the server's scratch git clones. The server will
-- | re-clone fresh copies on the next operation, ensuring a clean cache state.
resetGitFixtures :: E2E Unit
resetGitFixtures = do
  { stateDir } <- ask
  fixturesDir <- liftEffect $ Env.lookupRequired Env.repoFixturesDir
  let
    registryOrigin = Path.concat [ fixturesDir, "purescript", "registry" ]
    registryIndexOrigin = Path.concat [ fixturesDir, "purescript", "registry-index" ]
    scratchDir = Path.concat [ stateDir, "scratch" ]
  resetOrigin registryOrigin
  resetOrigin registryIndexOrigin
  deleteGitClones scratchDir
  where
  resetOrigin dir = do
    void $ gitOrFail [ "reset", "--hard", "initial-fixture" ] dir
    void $ gitOrFail [ "clean", "-fd" ] dir

  deleteGitClones scratchDir = do
    liftAff $ FS.Extra.remove $ Path.concat [ scratchDir, "registry" ]
    liftAff $ FS.Extra.remove $ Path.concat [ scratchDir, "registry-index" ]

-- | Clear server log files for test isolation.
-- | Deletes *.log files from the scratch/logs directory but preserves the directory itself.
resetLogs :: E2E Unit
resetLogs = do
  { stateDir } <- ask
  let logsDir = Path.concat [ stateDir, "scratch", "logs" ]
  let cmd = "rm -f '" <> logsDir <> "'/*.log 2>/dev/null || true"
  result <- liftAff $ _.getResult =<< Execa.execa "sh" [ "-c", cmd ] identity
  case result.exit of
    Normally _ -> pure unit
    _ -> pure unit

-- | Clear cached GitHub API requests from the scratch cache directory.
-- | This ensures each test makes fresh API calls rather than using cached responses.
resetGitHubRequestCache :: E2E Unit
resetGitHubRequestCache = do
  { stateDir } <- ask
  let cacheDir = Path.concat [ stateDir, "scratch", ".cache" ]
  liftAff $ Aff.attempt (FS.Aff.readdir cacheDir) >>= case _ of
    Left _ -> pure unit
    Right files -> for_ files \file ->
      when (String.Pattern "Request__" `String.contains` file) do
        FS.Extra.remove (Path.concat [ cacheDir, file ])

-- | Poll a job until completion, failing the test if the job fails.
-- | Prints error logs on failure for debugging.
pollJobOrFail :: V1.JobId -> E2E V1.Job
pollJobOrFail jobId = do
  job <- Client.pollJob jobId
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
pollJobExpectFailure :: V1.JobId -> E2E V1.Job
pollJobExpectFailure jobId = do
  job <- Client.pollJob jobId
  when (V1.jobInfo job).success do
    Assert.fail "Expected job to fail, but it succeeded"
  pure job

-- | Sign an unpublish operation using the pacchettibotti private key from environment.
signUnpublishOrFail :: UnpublishData -> E2E AuthenticatedData
signUnpublishOrFail unpublishData = do
  { privateKey } <- ask
  case Fixtures.signUnpublish privateKey unpublishData of
    Left err -> liftAff $ Aff.throwError $ Aff.error $ "Failed to sign unpublish: " <> err
    Right authData -> pure authData

-- | Sign a transfer operation using the pacchettibotti private key from environment.
signTransferOrFail :: TransferData -> E2E AuthenticatedData
signTransferOrFail transferData = do
  { privateKey } <- ask
  case Fixtures.signTransfer privateKey transferData of
    Left err -> liftAff $ Aff.throwError $ Aff.error $ "Failed to sign transfer: " <> err
    Right authData -> pure authData

-- | Run git status --porcelain in a directory and return the output.
gitStatus :: String -> E2E String
gitStatus cwd = gitOrFail [ "status", "--porcelain" ] cwd

-- | Run a git command, throwing an exception on failure.
gitOrFail :: Array String -> FilePath -> E2E String
gitOrFail args cwd = liftAff $ Git.gitCLI args (Just cwd) >>= case _ of
  Left err -> Aff.throwError $ Aff.error err
  Right out -> pure out

-- | Check if git status output indicates a clean working tree (no changes).
isCleanGitStatus :: String -> Boolean
isCleanGitStatus status = String.null status

-- | Wait for all matrix jobs for a package to complete.
waitForAllMatrixJobs :: PackageFixture -> E2E Unit
waitForAllMatrixJobs pkg = go 120 0
  where
  go :: Int -> Int -> E2E Unit
  go 0 _ = liftAff $ Aff.throwError $ Aff.error "Timed out waiting for matrix jobs to complete"
  go attempts lastCount = do
    jobs <- Client.getJobs
    let
      matrixJobs = Array.filter (isMatrixJobFor pkg) jobs
      totalCount = Array.length matrixJobs
      finishedCount = Array.length $ Array.filter (\j -> isJust (V1.jobInfo j).finishedAt) matrixJobs
      allFinished = finishedCount == totalCount
      stillCreating = totalCount > lastCount
    if totalCount >= 1 && allFinished && not stillCreating then
      pure unit
    else do
      when (attempts `mod` 10 == 0) do
        Console.log $ "Waiting for matrix jobs: " <> show finishedCount <> "/" <> show totalCount <> " finished"
      liftAff $ Aff.delay (Milliseconds 1000.0)
      go (attempts - 1) totalCount

-- | Check if a job is a matrix job for the given package.
isMatrixJobFor :: PackageFixture -> Job -> Boolean
isMatrixJobFor pkg = case _ of
  MatrixJob { packageName, packageVersion } ->
    packageName == pkg.name && packageVersion == pkg.version
  _ -> false

-- | Read and parse the metadata file for a package from the server's scratch clone.
readMetadata :: PackageName -> E2E Metadata
readMetadata packageName = do
  { stateDir } <- ask
  let metadataPath = Path.concat [ stateDir, "scratch", "registry", "metadata", PackageName.print packageName <> ".json" ]
  liftAff (readJsonFile Metadata.codec metadataPath) >>= case _ of
    Left err -> liftAff $ Aff.throwError $ Aff.error $ "Failed to read metadata for " <> PackageName.print packageName <> ": " <> err
    Right metadata -> pure metadata

-- | Read and parse the manifest index entry for a package from the server's scratch clone.
readManifestIndexEntry :: PackageName -> E2E (Array Manifest)
readManifestIndexEntry packageName = do
  { stateDir } <- ask
  let indexPath = Path.concat [ stateDir, "scratch", "registry-index" ]
  liftAff $ ManifestIndex.readEntryFile indexPath packageName >>= case _ of
    Left err -> Aff.throwError $ Aff.error $ "Failed to read manifest index for " <> PackageName.print packageName <> ": " <> err
    Right manifests -> pure $ Array.fromFoldable manifests

-- | Check if a specific package version exists in the manifest index.
manifestIndexEntryExists :: PackageFixture -> E2E Boolean
manifestIndexEntryExists pkg = do
  { stateDir } <- ask
  let indexPath = Path.concat [ stateDir, "scratch", "registry-index" ]
  liftAff $ ManifestIndex.readEntryFile indexPath pkg.name >>= case _ of
    Left _ -> pure false
    Right manifests -> pure $ Array.any (\(Manifest m) -> m.version == pkg.version) $ Array.fromFoldable manifests

-- | Assert that both git repos (registry and registry-index) have no uncommitted changes.
assertReposClean :: E2E Unit
assertReposClean = do
  { stateDir } <- ask
  let scratchRegistry = Path.concat [ stateDir, "scratch", "registry" ]
  let scratchRegistryIndex = Path.concat [ stateDir, "scratch", "registry-index" ]
  registryStatus <- gitStatus scratchRegistry
  registryIndexStatus <- gitStatus scratchRegistryIndex
  unless (isCleanGitStatus registryStatus) do
    Assert.fail $ "registry repo has uncommitted changes:\n" <> registryStatus
  unless (isCleanGitStatus registryIndexStatus) do
    Assert.fail $ "registry-index repo has uncommitted changes:\n" <> registryIndexStatus

-- | Check if a storage upload (PUT) occurred for a specific package.
hasStorageUpload :: PackageFixture -> E2E Boolean
hasStorageUpload pkg = do
  requests <- WireMock.getStorageRequests
  let
    expectedPath = PackageName.print pkg.name <> "/" <> Version.print pkg.version <> ".tar.gz"
    putRequests = WireMock.filterByMethod "PUT" requests
  pure $ Array.any (\r -> String.contains (String.Pattern expectedPath) r.url) putRequests

-- | Check if a storage delete (DELETE) occurred for a specific package.
hasStorageDelete :: PackageFixture -> E2E Boolean
hasStorageDelete pkg = do
  requests <- WireMock.getStorageRequests
  let
    expectedPath = PackageName.print pkg.name <> "/" <> Version.print pkg.version <> ".tar.gz"
    deleteRequests = WireMock.filterByMethod "DELETE" requests
  pure $ Array.any (\r -> String.contains (String.Pattern expectedPath) r.url) deleteRequests
