module Test.E2E.Endpoint.Transfer (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Registry.API.V1 as V1
import Registry.Location (Location(..))
import Registry.Metadata (Metadata(..))
import Registry.PackageName as PackageName
import Registry.Test.Assert as Assert
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env (E2ESpec)
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as Fixtures
import Test.E2E.Support.WireMock as WireMock
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.describe "Transfer workflow" do
    Spec.it "can transfer effect to a new location with full state verification" do
      { jobId: publishJobId } <- Client.publish Fixtures.effectPublishData
      _ <- Env.pollJobOrFail publishJobId
      Env.waitForAllMatrixJobs Fixtures.effect

      Metadata originalMetadata <- Env.readMetadata Fixtures.effect.name
      case originalMetadata.location of
        GitHub { owner } -> Assert.shouldEqual owner "purescript"
        Git _ -> Assert.fail "Expected GitHub location, got Git"

      -- clear the publish PUT so we can verify transfers leave storage unaffected
      WireMock.clearStorageRequests

      authData <- Env.signTransferOrFail Fixtures.effectTransferData
      { jobId: transferJobId } <- Client.transfer authData
      transferJob <- Env.pollJobOrFail transferJobId
      Assert.shouldSatisfy (V1.jobInfo transferJob).finishedAt isJust

      Metadata newMetadata <- Env.readMetadata Fixtures.effect.name
      case newMetadata.location of
        GitHub { owner } -> Assert.shouldEqual owner "new-owner"
        Git _ -> Assert.fail "Expected GitHub location after transfer, got Git"

      storageRequests <- WireMock.getStorageRequests
      let
        packagePath = PackageName.print Fixtures.effect.name
        putOrDeleteRequests = Array.filter
          (\r -> (r.method == "PUT" || r.method == "DELETE") && WireMock.filterByUrlContaining packagePath [ r ] /= [])
          storageRequests
      unless (Array.null putOrDeleteRequests) do
        WireMock.failWithRequests "Transfer should not PUT or DELETE to storage" putOrDeleteRequests
