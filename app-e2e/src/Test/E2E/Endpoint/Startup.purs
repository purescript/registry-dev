-- | E2E tests for the Scheduler and JobExecutor startup, covering:
-- | - scheduleLegacyImports: Detects new package versions via GitHub tags
-- | - scheduleTransfers: Detects packages that moved to new GitHub locations
-- | - schedulePackageSetUpdates: Detects recent uploads for package set inclusion
-- | - checkIfNewCompiler: Detects new compiler and enqueues matrix jobs
-- |
-- | IMPORTANT: These tests must run BEFORE resetTestState is called, since
-- | the scheduler runs at server startup and creates jobs that would be cleared.
module Test.E2E.Endpoint.Startup (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Map as Map
import Registry.API.V1 (Job(..))
import Registry.Location (Location(..))
import Registry.Operation (AuthenticatedPackageOperation(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Test.Assert as Assert
import Registry.Version as Version
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env (E2ESpec)
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.describe "scheduleLegacyImports" do
    Spec.it "enqueues publish jobs for new package versions discovered via GitHub tags" do
      -- The scheduler runs at server startup and should have already
      -- fetched tags for packages in the registry metadata.
      -- type-equality has v4.0.1 published but v4.0.2 in tags (per wiremock config)
      jobs <- Client.getJobs

      -- Find publish jobs for type-equality
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
          -- Verify the compiler is from previous version (type-equality@4.0.1 has compilers [0.15.10, 0.15.11])
          -- The scheduler should use the lowest compiler from the previous version for compatibility
          let expectedCompiler = unsafeFromRight (Version.parse "0.15.10")
          when (payload.compiler /= expectedCompiler) do
            Assert.fail $ "Expected compiler 0.15.10 but got " <> Version.print payload.compiler
        Just _ -> Assert.fail "Expected PublishJob but got different job type"
        Nothing -> do
          -- Log what jobs we did find for debugging
          let publishJobs = Array.filter isPublishJob jobs
          Assert.fail $ "Expected to find a publish job for type-equality@4.0.2 but found "
            <> show (Array.length publishJobs)
            <> " publish jobs: "
            <> show (map formatPublishJob publishJobs)

    Spec.it "does not enqueue jobs for already-published versions" do
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

  Spec.describe "scheduleTransfers" do
    Spec.it "enqueues transfer jobs when package location changes" do
      -- type-equality metadata says old-owner, but tags point to purescript
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
                  when (owner /= "purescript") do
                    Assert.fail $ "Expected owner 'purescript' but got '" <> owner <> "'"
                _ -> Assert.fail "Expected GitHub location"
            _ -> Assert.fail "Expected Transfer payload"
        Just _ -> Assert.fail "Expected TransferJob but got different job type"
        Nothing -> do
          let transferJobs = Array.filter isTransferJob jobs
          Assert.fail $ "Expected to find a transfer job for 'type-equality' but found "
            <> show (Array.length transferJobs)
            <> " transfer jobs"

  Spec.describe "schedulePackageSetUpdates" do
    Spec.it "enqueues package set update for recent uploads not in set" do
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

  Spec.describe "checkIfNewCompiler" do
    Spec.it "enqueues matrix jobs for packages with no dependencies when new compiler detected" do
      -- The test env has compilers 0.15.10 and 0.15.11 available.
      -- prelude@6.0.1 fixture only has compiler 0.15.10 in metadata.
      -- So 0.15.11 should be detected as "new" at startup, triggering
      -- matrix jobs for packages with no dependencies.
      jobs <- Client.getJobs
      let
        isNewCompilerMatrixJob :: Job -> Boolean
        isNewCompilerMatrixJob = case _ of
          MatrixJob { compilerVersion } ->
            compilerVersion == unsafeFromRight (Version.parse "0.15.11")
          _ -> false

        matrixJobs = Array.filter isNewCompilerMatrixJob jobs

        -- Get package names from matrix jobs
        matrixPackages = Array.mapMaybe
          ( \j -> case j of
              MatrixJob { packageName } -> Just packageName
              _ -> Nothing
          )
          matrixJobs

      -- Should have matrix jobs for packages with no dependencies
      -- prelude has no dependencies, so it should get a matrix job
      let preludeName = unsafeFromRight (PackageName.parse "prelude")
      unless (Array.elem preludeName matrixPackages) do
        Assert.fail $ "Expected matrix job for prelude with compiler 0.15.11, found: "
          <> show (Array.length matrixJobs)
          <> " matrix jobs for packages: "
          <> show (map PackageName.print matrixPackages)

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
