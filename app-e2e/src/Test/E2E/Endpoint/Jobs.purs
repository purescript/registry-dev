module Test.E2E.Endpoint.Jobs (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Registry.API.V1 (Job(..), JobId(..))
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
    Spec.it "lists jobs and respects include_completed filter" do
      { jobId: publishJobId } <- Client.publish Fixtures.effectPublishData
      _ <- Env.pollJobOrFail publishJobId
      Env.waitForAllMatrixJobs Fixtures.effect

      recentJobs <- Client.getJobsWith Client.ActiveOnly
      allJobs <- Client.getJobsWith Client.IncludeCompleted

      let allCount = Array.length allJobs
      Assert.shouldSatisfy allCount (_ > 0)

      let recentCount = Array.length recentJobs
      Assert.shouldSatisfy recentCount (_ <= allCount)

      let
        isEffectMatrixJob = case _ of
          MatrixJob { packageName } -> packageName == Fixtures.effect.name
          _ -> false
        effectMatrixJobs = Array.filter isEffectMatrixJob allJobs
      Assert.shouldSatisfy (Array.length effectMatrixJobs) (_ > 0)

      let completedJob = Array.find (\job -> isJust (V1.jobInfo job).finishedAt) allJobs
      case completedJob of
        Just completed -> do
          let
            jobId = (V1.jobInfo completed).jobId
            inRecent = Array.any (\job -> (V1.jobInfo job).jobId == jobId) recentJobs
          when inRecent do
            Assert.fail $ "Completed job " <> unwrap jobId <> " should be excluded from include_completed=false results"
        Nothing -> pure unit

  Spec.describe "Jobs API error handling" do
    Spec.it "returns HTTP 404 for non-existent job ID" do
      let fakeJobId = JobId "nonexistent-job-id-12345"
      result <- Client.tryGetJob fakeJobId Nothing Nothing Nothing
      case result of
        Right _ ->
          Assert.fail "Expected HTTP 404 for non-existent job"
        Left err ->
          case Client.clientErrorStatus err of
            Just 404 -> pure unit
            _ -> Assert.fail $ "Expected HTTP 404, got: " <> Client.printClientError err
