module Test.E2E.Endpoint.Unpublish (spec) where

import Registry.App.Prelude

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
    Spec.it "can publish effect@4.0.0 then unpublish it with full state verification" do
      { jobId: publishJobId } <- Client.publish Fixtures.effectPublishData
      _ <- Env.pollJobOrFail publishJobId
      -- Note: we don't wait for matrix jobs - unpublish only modifies metadata/storage

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
