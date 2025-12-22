-- | End-to-end tests for the Publish API endpoint.
-- | These tests exercise the actual registry server via HTTP requests.
module Test.E2E.Publish (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Registry.API.V1 as V1
import Registry.Location as Registry.Location
import Registry.Test.Assert as Assert
import Registry.Test.E2E.Client as Client
import Registry.Test.Utils as Utils
import Test.Spec (Spec)
import Test.Spec as Spec

-- | Get client config from environment
getConfig :: Aff Client.Config
getConfig = liftEffect Client.configFromEnv

spec :: Spec Unit
spec = do
  Spec.describe "Server connectivity" do
    Spec.it "can reach the status endpoint" do
      config <- getConfig
      result <- Client.getStatus config
      case result of
        Left err -> Assert.fail $ "Failed to reach status endpoint: " <> Client.printClientError err
        Right _ -> pure unit

    Spec.it "can list jobs (initially empty)" do
      config <- getConfig
      result <- Client.getJobs config
      case result of
        Left err -> Assert.fail $ "Failed to list jobs: " <> Client.printClientError err
        Right _ -> pure unit -- Jobs list may not be empty if other tests ran

  Spec.describe "Publish workflow" do
    Spec.it "can publish effect@4.0.0" do
      config <- getConfig
      let
        -- Location must match what's in the fixture metadata
        effectLocation = Registry.Location.GitHub
          { owner: "purescript"
          , repo: "purescript-effect"
          , subdir: Nothing
          }
        publishData =
          { name: Utils.unsafePackageName "effect"
          , location: Just effectLocation
          , ref: "v4.0.0"
          , compiler: Utils.unsafeVersion "0.15.9"
          , resolutions: Nothing
          , version: Utils.unsafeVersion "4.0.0"
          }

      -- Submit publish request
      publishResult <- Client.publish config publishData
      case publishResult of
        Left err -> Assert.fail $ "Failed to submit publish request: " <> Client.printClientError err
        Right { jobId } -> do
          -- Poll until job completes
          job <- Client.pollJob config jobId

          -- If job failed, print logs for debugging
          unless (V1.jobInfo job).success do
            Console.log "Job failed! Logs:"
            let logMessages = map (\l -> "[" <> V1.printLogLevel l.level <> "] " <> l.message) (V1.jobInfo job).logs
            Console.log $ String.joinWith "\n" logMessages

          -- Verify job completed successfully
          when (not (V1.jobInfo job).success) do
            let errorLogs = Array.filter (\l -> l.level == V1.Error) (V1.jobInfo job).logs
            let errorMessages = map _.message errorLogs
            Assert.fail $ "Job failed with errors:\n" <> String.joinWith "\n" errorMessages

          Assert.shouldSatisfy (V1.jobInfo job).finishedAt isJust
-- Assert.shouldEqual job.jobType JobType.PublishJob
-- Assert.shouldEqual job.packageName (Utils.unsafePackageName "effect")
-- Assert.shouldEqual job.ref "v4.0.0"
