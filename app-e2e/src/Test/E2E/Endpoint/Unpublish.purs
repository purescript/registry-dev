module Test.E2E.Endpoint.Unpublish (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Registry.API.V1 as V1
import Registry.Metadata (Metadata(..))
import Registry.Test.Assert as Assert
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env (E2ESpec)
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as Fixtures
import Test.E2E.Support.WireMock as WireMock
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.describe "Publish-Unpublish workflow" do
    Spec.it "can publish then unpublish with full state verification" do
      { jobId: publishJobId } <- Client.publish Fixtures.effectPublishData
      _ <- Env.pollJobOrFail publishJobId

      existsBefore <- Env.manifestIndexEntryExists Fixtures.effect
      unless existsBefore do
        Assert.fail "Expected version to exist in manifest index before unpublish"

      authData <- Env.signUnpublishOrFail Fixtures.effectUnpublishData
      { jobId: unpublishJobId } <- Client.unpublish authData
      unpublishJob <- Env.pollJobOrFail unpublishJobId
      Assert.shouldSatisfy (V1.jobInfo unpublishJob).finishedAt isJust

      Metadata metadata <- Env.readMetadata Fixtures.effect.name

      case Map.lookup Fixtures.effect.version metadata.unpublished of
        Nothing ->
          Assert.fail "Expected version 4.0.0 to be in 'unpublished' metadata"
        Just unpublishedInfo ->
          Assert.shouldSatisfy unpublishedInfo.reason (not <<< String.null)

      when (Map.member Fixtures.effect.version metadata.published) do
        Assert.fail "Version 4.0.0 should not be in 'published' metadata after unpublish"

      deleteOccurred <- Env.hasStorageDelete Fixtures.effect
      unless deleteOccurred do
        storageRequests <- WireMock.getStorageRequests
        WireMock.failWithRequests "Expected S3 DELETE for effect/4.0.0.tar.gz" storageRequests

      existsAfter <- Env.manifestIndexEntryExists Fixtures.effect
      when existsAfter do
        Assert.fail "Expected version to be removed from manifest index after unpublish"

    -- Test race condition: submit unpublish while publish is still running.
    -- Job priority (Unpublish > Matrix) ensures unpublish runs before matrix jobs.
    Spec.it "unpublishing before matrix jobs complete causes them to fail gracefully" do
      -- Submit publish, don't wait for it to complete
      { jobId: publishJobId } <- Client.publish Fixtures.effectPublishData

      -- Immediately submit unpublish - it will be queued and run after publish
      -- but BEFORE matrix jobs due to job priority ordering
      authData <- Env.signUnpublishOrFail Fixtures.effectUnpublishData
      { jobId: unpublishJobId } <- Client.unpublish authData

      -- Now wait for publish to complete
      _ <- Env.pollJobOrFail publishJobId

      -- Wait for unpublish to complete
      unpublishJob <- Env.pollJobOrFail unpublishJobId
      Assert.shouldSatisfy (V1.jobInfo unpublishJob).finishedAt isJust

      -- Verify unpublish succeeded
      Metadata metadata <- Env.readMetadata Fixtures.effect.name
      case Map.lookup Fixtures.effect.version metadata.unpublished of
        Nothing ->
          Assert.fail "Expected version 4.0.0 to be in 'unpublished' metadata"
        Just _ -> pure unit

      -- Wait for matrix jobs to complete
      Env.waitForAllMatrixJobs Fixtures.effect

      -- Verify matrix jobs failed (they tried to download deleted tarball)
      jobs <- Client.getJobs
      let
        matrixJobs = Array.filter (Env.isMatrixJobFor Fixtures.effect) jobs
        allFailed = Array.all (\j -> not (V1.jobInfo j).success) matrixJobs

      unless (Array.null matrixJobs || allFailed) do
        Assert.fail "Expected matrix jobs to fail after unpublish deleted the tarball"

      -- Critical: verify no bad writes occurred - the version should NOT be
      -- back in published metadata (Map.update on missing key is a no-op)
      Metadata metadataAfterMatrix <- Env.readMetadata Fixtures.effect.name
      when (Map.member Fixtures.effect.version metadataAfterMatrix.published) do
        Assert.fail "Matrix job incorrectly wrote to published metadata for unpublished version"
