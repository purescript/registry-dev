module Test.E2E.Endpoint.Jobs (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Registry.API.V1 (JobId(..))
import Registry.API.V1 as V1
import Registry.Test.Assert as Assert
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env (E2ESpec)
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as Fixtures
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.describe "Status endpoint" do
    Spec.it "can reach the status endpoint" do
      Client.getStatus

  Spec.describe "Jobs list" do
    Spec.it "excludes completed jobs when include_completed is false" do
      -- Create a job and wait for it to complete
      { jobId } <- Client.publish Fixtures.effectPublishData
      _ <- Env.pollJobOrFail jobId

      -- Now we have at least one completed job
      recentJobs <- Client.getJobsWith Client.ActiveOnly
      allJobs <- Client.getJobsWith Client.IncludeCompleted

      -- All jobs should include the completed publish job
      let allCount = Array.length allJobs
      Assert.shouldSatisfy allCount (_ > 0)

      -- Active-only should return fewer or equal jobs
      let recentCount = Array.length recentJobs
      Assert.shouldSatisfy recentCount (_ <= allCount)

      -- Verify completed jobs are excluded from active-only results
      let completedJob = Array.find (\job -> isJust (V1.jobInfo job).finishedAt) allJobs
      case completedJob of
        Just completed -> do
          let
            completedId = (V1.jobInfo completed).jobId
            inRecent = Array.any (\job -> (V1.jobInfo job).jobId == completedId) recentJobs
          when inRecent do
            Assert.fail $ "Completed job " <> unwrap completedId <> " should be excluded from include_completed=false results"
        Nothing -> pure unit

  Spec.describe "Job query parameters" do
    Spec.it "accepts level and since parameters" do
      { jobId } <- Client.publish Fixtures.effectPublishData
      job <- Env.pollJobOrFail jobId
      let info = V1.jobInfo job

      baseJob <- Client.getJob jobId Nothing Nothing
      Assert.shouldEqual (V1.jobInfo baseJob).jobId info.jobId

      debugJob <- Client.getJob jobId (Just V1.Debug) Nothing
      Assert.shouldEqual (V1.jobInfo debugJob).jobId info.jobId

      let sinceTime = fromMaybe info.createdAt info.finishedAt
      sinceJob <- Client.getJob jobId Nothing (Just sinceTime)
      Assert.shouldEqual (V1.jobInfo sinceJob).jobId info.jobId

  Spec.describe "Jobs API error handling" do
    Spec.it "returns HTTP 404 for non-existent job ID" do
      let fakeJobId = JobId "nonexistent-job-id-12345"
      result <- Client.tryGetJob fakeJobId Nothing Nothing
      case result of
        Right _ ->
          Assert.fail "Expected HTTP 404 for non-existent job"
        Left err ->
          case Client.clientErrorStatus err of
            Just 404 -> pure unit
            _ -> Assert.fail $ "Expected HTTP 404, got: " <> Client.printClientError err
