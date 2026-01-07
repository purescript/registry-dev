module Test.E2E.Endpoint.Jobs (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Registry.API.V1 (Job(..), JobId(..))
import Registry.API.V1 as V1
import Registry.Test.Assert as Assert
import Registry.Test.Fixtures as Fixtures
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env as Env
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "Status endpoint" do
    Spec.it "can reach the status endpoint" do
      config <- Env.getConfig
      _ <- Env.expectRight "reach status endpoint" =<< Client.getStatus config
      pure unit

  Spec.describe "Jobs list" do
    Spec.it "lists jobs and respects include_completed filter" do
      config <- Env.getConfig

      -- Get jobs with and without include_completed
      recentJobs <- Env.expectRight "get recent jobs" =<< Client.getJobsWith Client.ActiveOnly config
      allJobs <- Env.expectRight "get all jobs" =<< Client.getJobsWith Client.IncludeCompleted config

      let
        recentCount = Array.length recentJobs
        allCount = Array.length allJobs

      Assert.shouldSatisfy allCount (_ > 0)
      Assert.shouldSatisfy recentCount (_ <= allCount)

      -- Verify seeded matrix jobs exist (prelude and type-equality are seeded by test env)
      let
        seededPackages = [ Fixtures.prelude.name, Fixtures.typeEquality.name ]
        isSeededMatrixJob j = case j of
          MatrixJob { packageName } -> Array.elem packageName seededPackages
          _ -> false
        seededJobs = Array.filter isSeededMatrixJob allJobs
      Assert.shouldSatisfy (Array.length seededJobs) (_ > 0)

      -- Verify completed jobs are excluded when include_completed=false
      let completedJob = Array.find (\j -> isJust (V1.jobInfo j).finishedAt) allJobs
      case completedJob of
        Just job -> do
          let
            jobId = (V1.jobInfo job).jobId
            inRecent = Array.any (\j -> (V1.jobInfo j).jobId == jobId) recentJobs
          when inRecent do
            Assert.fail $ "Completed job " <> unwrap jobId <> " should be excluded from include_completed=false results"
        Nothing -> pure unit

  Spec.describe "Jobs API error handling" do
    Spec.it "returns HTTP 404 for non-existent job ID" do
      config <- Env.getConfig
      let fakeJobId = JobId "nonexistent-job-id-12345"
      result <- Client.getJob config fakeJobId Nothing Nothing Nothing
      case result of
        Right _ ->
          Assert.fail "Expected HTTP 404 for non-existent job"
        Left (Client.HttpError { status }) ->
          Assert.shouldEqual status 404
        Left err ->
          Assert.fail $ "Expected HTTP 404, got: " <> Client.printClientError err
