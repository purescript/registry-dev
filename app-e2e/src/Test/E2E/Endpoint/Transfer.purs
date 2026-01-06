module Test.E2E.Endpoint.Transfer (spec) where

import Registry.App.Prelude

import Registry.API.V1 as V1
import Registry.Location (Location(..))
import Registry.Metadata (Metadata(..))
import Registry.Test.Assert as Assert
import Registry.Test.Utils (unsafePackageName)
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as Fixtures
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "Transfer workflow" do
    Spec.before_ Env.resetTestState do

      Spec.it "can transfer effect to a new location" do
        config <- Env.getConfig

        -- Publish first (need a published package to transfer)
        { jobId: publishJobId } <- Env.expectRight "publish effect" =<< Client.publish config Fixtures.effectPublishData
        _ <- Env.pollJobOrFail config publishJobId

        -- Verify original location
        Metadata originalMetadata <- Env.readMetadata (unsafePackageName "effect")
        case originalMetadata.location of
          GitHub { owner } -> Assert.shouldEqual owner "purescript"
          Git _ -> Assert.fail "Expected GitHub location, got Git"

        -- Transfer
        authData <- Env.signTransferOrFail Fixtures.effectTransferData
        { jobId: transferJobId } <- Env.expectRight "submit transfer" =<< Client.transfer config authData
        transferJob <- Env.pollJobOrFail config transferJobId
        Assert.shouldSatisfy (V1.jobInfo transferJob).finishedAt isJust

        -- Verify location changed
        Metadata newMetadata <- Env.readMetadata (unsafePackageName "effect")
        case newMetadata.location of
          GitHub { owner } -> Assert.shouldEqual owner "new-owner"
          Git _ -> Assert.fail "Expected GitHub location after transfer, got Git"
