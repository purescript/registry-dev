module Test.E2E.Endpoint.Unpublish (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Registry.API.V1 as V1
import Registry.Metadata (Metadata(..))
import Registry.Test.Assert as Assert
import Registry.Test.Utils (unsafePackageName, unsafeVersion)
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as Fixtures
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "Publish-Unpublish workflow" do
    Spec.before_ Env.resetTestState do

      Spec.it "can publish effect@4.0.0 then unpublish it" do
        config <- Env.getConfig

        -- Publish
        { jobId: publishJobId } <- Env.expectRight "submit publish" =<< Client.publish config Fixtures.effectPublishData
        _ <- Env.pollJobOrFail config publishJobId

        -- Unpublish
        authData <- Env.signUnpublishOrFail Fixtures.effectUnpublishData
        { jobId: unpublishJobId } <- Env.expectRight "submit unpublish" =<< Client.unpublish config authData
        unpublishJob <- Env.pollJobOrFail config unpublishJobId
        Assert.shouldSatisfy (V1.jobInfo unpublishJob).finishedAt isJust

        -- Verify metadata shows version as unpublished
        Metadata metadata <- Env.readMetadata (unsafePackageName "effect")
        let version = unsafeVersion "4.0.0"

        case Map.lookup version metadata.unpublished of
          Nothing ->
            Assert.fail "Expected version 4.0.0 to be in 'unpublished' metadata"
          Just unpublishedInfo ->
            Assert.shouldSatisfy unpublishedInfo.reason (not <<< String.null)

        when (Map.member version metadata.published) do
          Assert.fail "Version 4.0.0 should not be in 'published' metadata after unpublish"

        -- Verify S3 DELETE was called
        storageRequests <- Env.getStorageRequests
        let deleteRequests = Array.filter (\r -> r.method == "DELETE" && String.contains (String.Pattern "effect") r.url) storageRequests
        when (Array.null deleteRequests) do
          Assert.fail "Expected S3 DELETE request for effect tarball"
