-- | End-to-end tests for the cronjob scripts:
-- | - DailyImporter: Detects new package versions via GitHub tags
-- | - PackageSetUpdater: Detects recent uploads for package set inclusion
-- | - PackageTransferrer: Detects packages that moved to new GitHub locations
-- |
-- | These tests verify that the scripts properly enqueue jobs via the API.
module Test.E2E.Scripts (spec) where

import Registry.App.Prelude

import Control.Monad.Reader (ask)
import Data.Array as Array
import Data.Map as Map
import Effect.Aff as Aff
import Node.Path as Path
import Node.Process as Process
import Registry.API.V1 (Job(..))
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry as Registry
import Registry.Foreign.Octokit as Octokit
import Registry.Location (Location(..))
import Registry.Operation (AuthenticatedPackageOperation(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Scripts.DailyImporter as DailyImporter
import Registry.Scripts.PackageSetUpdater as PackageSetUpdater
import Registry.Scripts.PackageTransferrer as PackageTransferrer
import Registry.Test.Assert as Assert
import Registry.Version as Version
import Run as Run
import Run.Except as Except
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env (E2E, E2ESpec)
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.describe "DailyImporter" do
    Spec.it "enqueues publish jobs for new package versions discovered via GitHub tags" do
      runDailyImporterScript
      jobs <- Client.getJobs

      -- type-equality has v4.0.1 published but v4.0.2 in tags (per wiremock config)
      let
        isTypeEqualityPublishJob :: Job -> Boolean
        isTypeEqualityPublishJob = case _ of
          PublishJob { packageName, packageVersion } ->
            packageName == unsafeFromRight (PackageName.parse "type-equality")
              && packageVersion
              == unsafeFromRight (Version.parse "4.0.2")
          _ -> false

        typeEqualityJob = Array.find isTypeEqualityPublishJob jobs

      case typeEqualityJob of
        Just (PublishJob { payload }) -> do
          -- Verify compiler selection logic
          let expectedCompiler = unsafeFromRight (Version.parse "0.15.10")
          when (payload.compiler /= expectedCompiler) do
            Assert.fail $ "Expected compiler 0.15.10 but got " <> Version.print payload.compiler
        Just _ -> Assert.fail "Expected PublishJob but got different job type"
        Nothing -> do
          let publishJobs = Array.filter isPublishJob jobs
          Assert.fail $ "Expected to find a publish job for type-equality@4.0.2 but found "
            <> show (Array.length publishJobs)
            <> " publish jobs: "
            <> show (map formatPublishJob publishJobs)

    Spec.it "does not enqueue jobs for already-published versions" do
      runDailyImporterScript
      jobs <- Client.getJobs

      -- type-equality v4.0.1 is already published, should NOT have a new job
      let
        isDuplicateJob :: Job -> Boolean
        isDuplicateJob = case _ of
          PublishJob { packageName, packageVersion } ->
            packageName == unsafeFromRight (PackageName.parse "type-equality")
              && packageVersion
              == unsafeFromRight (Version.parse "4.0.1")
          _ -> false

        duplicateJob = Array.find isDuplicateJob jobs

      case duplicateJob of
        Nothing -> pure unit -- Good, no duplicate job
        Just _ -> Assert.fail "Found unexpected publish job for already-published type-equality@4.0.1"

  Spec.describe "PackageTransferrer" do
    Spec.it "enqueues transfer jobs when package location changes" do
      runPackageTransferrerScript
      -- type-equality metadata says purescript, but tags point to new-owner
      jobs <- Client.getJobs
      let
        isTypeEqualityTransferJob :: Job -> Boolean
        isTypeEqualityTransferJob = case _ of
          TransferJob { packageName } ->
            packageName == unsafeFromRight (PackageName.parse "type-equality")
          _ -> false
      case Array.find isTypeEqualityTransferJob jobs of
        Just (TransferJob { packageName, payload }) -> do
          -- Verify packageName
          when (packageName /= unsafeFromRight (PackageName.parse "type-equality")) do
            Assert.fail $ "Wrong package name: " <> PackageName.print packageName
          -- Verify newLocation in payload
          case payload.payload of
            Transfer { newLocation } ->
              case newLocation of
                GitHub { owner } ->
                  when (owner /= "new-owner") do
                    Assert.fail $ "Expected owner 'new-owner' but got '" <> owner <> "'"
                _ -> Assert.fail "Expected GitHub location"
            _ -> Assert.fail "Expected Transfer payload"
        Just _ -> Assert.fail "Expected TransferJob but got different job type"
        Nothing -> do
          let transferJobs = Array.filter isTransferJob jobs
          Assert.fail $ "Expected to find a transfer job for 'type-equality' but found "
            <> show (Array.length transferJobs)
            <> " transfer jobs"

  Spec.describe "PackageSetUpdater" do
    Spec.it "enqueues package set update for recent uploads not in set" do
      runPackageSetUpdaterScript
      jobs <- Client.getJobs
      let packageSetJobs = Array.filter isPackageSetJob jobs
      case Array.head packageSetJobs of
        Just (PackageSetJob { payload }) ->
          case payload of
            Operation.PackageSetUpdate { packages } ->
              case Map.lookup (unsafeFromRight $ PackageName.parse "type-equality") packages of
                Just (Just _) -> pure unit
                _ -> Assert.fail "Expected type-equality in package set update"
        Just _ -> Assert.fail "Expected PackageSetJob but got different job type"
        Nothing -> Assert.fail "Expected package set job to be enqueued"

-- | Run the DailyImporter script in Submit mode
runDailyImporterScript :: E2E Unit
runDailyImporterScript = do
  { stateDir } <- ask

  -- Set up environment
  liftEffect $ Process.chdir stateDir

  -- Get resource env from environment variables
  resourceEnv <- liftEffect Env.lookupResourceEnv
  token <- liftEffect $ Env.lookupRequired Env.githubToken

  githubCacheRef <- liftAff Cache.newCacheRef
  registryCacheRef <- liftAff Cache.newCacheRef
  let cache = Path.concat [ stateDir, "scratch", ".cache" ]

  octokit <- liftAff $ Octokit.newOctokit token resourceEnv.githubApiUrl
  debouncer <- liftAff Registry.newDebouncer

  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { pull: Git.Autostash
      , write: Registry.ReadOnly
      , repos: Registry.defaultRepos
      , workdir: Path.concat [ stateDir, "scratch" ]
      , debouncer
      , cacheRef: registryCacheRef
      }

  result <- liftAff
    $ DailyImporter.runDailyImport DailyImporter.Submit resourceEnv.registryApiUrl
    # Except.runExcept
    # Registry.interpret (Registry.handle registryEnv)
    # GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
    # Log.interpret (Log.handleTerminal Quiet)
    # Env.runResourceEnv resourceEnv
    # Run.runBaseAff'

  case result of
    Left err -> liftAff $ Aff.throwError $ Aff.error $ "DailyImporter failed: " <> err
    Right _ -> pure unit

-- | Run the PackageTransferrer script in Submit mode
runPackageTransferrerScript :: E2E Unit
runPackageTransferrerScript = do
  { stateDir, privateKey } <- ask

  -- Set up environment
  liftEffect $ Process.chdir stateDir

  -- Get resource env from environment variables
  resourceEnv <- liftEffect Env.lookupResourceEnv
  token <- liftEffect $ Env.lookupRequired Env.githubToken

  githubCacheRef <- liftAff Cache.newCacheRef
  registryCacheRef <- liftAff Cache.newCacheRef
  let cache = Path.concat [ stateDir, "scratch", ".cache" ]

  octokit <- liftAff $ Octokit.newOctokit token resourceEnv.githubApiUrl
  debouncer <- liftAff Registry.newDebouncer

  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { pull: Git.Autostash
      , write: Registry.ReadOnly
      , repos: Registry.defaultRepos
      , workdir: Path.concat [ stateDir, "scratch" ]
      , debouncer
      , cacheRef: registryCacheRef
      }

  result <- liftAff
    $ PackageTransferrer.runPackageTransferrer PackageTransferrer.Submit (Just privateKey) resourceEnv.registryApiUrl
    # Except.runExcept
    # Registry.interpret (Registry.handle registryEnv)
    # GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
    # Log.interpret (Log.handleTerminal Quiet)
    # Env.runResourceEnv resourceEnv
    # Run.runBaseAff'

  case result of
    Left err -> liftAff $ Aff.throwError $ Aff.error $ "PackageTransferrer failed: " <> err
    Right _ -> pure unit

-- | Run the PackageSetUpdater script in Submit mode
runPackageSetUpdaterScript :: E2E Unit
runPackageSetUpdaterScript = do
  { stateDir, privateKey } <- ask

  -- Set up environment
  liftEffect $ Process.chdir stateDir

  -- Get resource env from environment variables
  resourceEnv <- liftEffect Env.lookupResourceEnv
  token <- liftEffect $ Env.lookupRequired Env.githubToken

  githubCacheRef <- liftAff Cache.newCacheRef
  registryCacheRef <- liftAff Cache.newCacheRef
  let cache = Path.concat [ stateDir, "scratch", ".cache" ]

  octokit <- liftAff $ Octokit.newOctokit token resourceEnv.githubApiUrl
  debouncer <- liftAff Registry.newDebouncer

  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { pull: Git.Autostash
      , write: Registry.ReadOnly
      , repos: Registry.defaultRepos
      , workdir: Path.concat [ stateDir, "scratch" ]
      , debouncer
      , cacheRef: registryCacheRef
      }

  result <- liftAff
    $ PackageSetUpdater.runPackageSetUpdater PackageSetUpdater.Submit (Just privateKey) resourceEnv.registryApiUrl
    # Except.runExcept
    # Registry.interpret (Registry.handle registryEnv)
    # GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
    # Log.interpret (Log.handleTerminal Quiet)
    # Env.runResourceEnv resourceEnv
    # Run.runBaseAff'

  case result of
    Left err -> liftAff $ Aff.throwError $ Aff.error $ "PackageSetUpdater failed: " <> err
    Right _ -> pure unit

-- | Check if a job is a PublishJob
isPublishJob :: Job -> Boolean
isPublishJob = case _ of
  PublishJob _ -> true
  _ -> false

-- | Format a PublishJob for debugging output
formatPublishJob :: Job -> String
formatPublishJob = case _ of
  PublishJob { packageName, packageVersion } ->
    PackageName.print packageName <> "@" <> Version.print packageVersion
  _ -> "<not a publish job>"

-- | Check if a job is a TransferJob
isTransferJob :: Job -> Boolean
isTransferJob = case _ of
  TransferJob _ -> true
  _ -> false

-- | Check if a job is a PackageSetJob
isPackageSetJob :: Job -> Boolean
isPackageSetJob = case _ of
  PackageSetJob _ -> true
  _ -> false
