module Test.E2E.Endpoint.Publish (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Registry.API.V1 as V1
import Registry.Test.Assert as Assert
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as Fixtures
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "Publish workflow" do
    Spec.before_ Env.resetTestState do

      Spec.it "can publish effect@4.0.0 and filter logs" do
        config <- Env.getConfig

        { jobId } <- Env.expectRight "submit publish" =<< Client.publish config Fixtures.effectPublishData
        job <- Env.pollJobOrFail config jobId
        Assert.shouldSatisfy (V1.jobInfo job).finishedAt isJust

        -- Verify log level filtering
        allLogsJob <- Env.expectRight "get job with DEBUG level" =<< Client.getJob config jobId (Just V1.Debug) Nothing Nothing
        let allLogs = (V1.jobInfo allLogsJob).logs

        infoLogsJob <- Env.expectRight "get job with INFO level" =<< Client.getJob config jobId (Just V1.Info) Nothing Nothing
        let
          infoLogs = (V1.jobInfo infoLogsJob).logs
          debugOnlyLogs = Array.filter (\l -> l.level == V1.Debug) allLogs
          infoContainsDebug = Array.any (\l -> l.level == V1.Debug) infoLogs
        when infoContainsDebug do
          Assert.fail "INFO level filter returned DEBUG logs"

        when (Array.length debugOnlyLogs > 0) do
          Assert.shouldSatisfy (Array.length infoLogs) (_ < Array.length allLogs)

        -- Verify timestamp filtering excludes earlier logs
        -- Use a high limit to ensure we get all logs for accurate count comparison
        allLogsUnlimited <- Env.expectRight "get all logs" =<< Client.getJob config jobId (Just V1.Debug) Nothing (Just 10000)
        let logs = (V1.jobInfo allLogsUnlimited).logs
        for_ (Array.index logs 1) \middleLog -> do
          sinceJob <- Env.expectRight "get job with since filter" =<< Client.getJob config jobId (Just V1.Debug) (Just middleLog.timestamp) (Just 10000)
          let sinceLogs = (V1.jobInfo sinceJob).logs
          -- All returned logs should be at or after the filter timestamp
          for_ sinceLogs \l ->
            Assert.shouldSatisfy l.timestamp (_ >= middleLog.timestamp)
          -- Verify we actually got some logs back (the filter is working, not just empty)
          Assert.shouldSatisfy (Array.length sinceLogs) (_ > 0)
          -- The since filter should return fewer logs than the full set
          Assert.shouldSatisfy (Array.length sinceLogs) (_ < Array.length logs)

  Spec.describe "Publish state machine" do
    Spec.it "returns same jobId for duplicate publish requests" do
      config <- Env.getConfig

      -- Submit same publish request twice
      { jobId: id1 } <- Env.expectRight "first publish" =<< Client.publish config Fixtures.effectPublishData
      { jobId: id2 } <- Env.expectRight "second publish" =<< Client.publish config Fixtures.effectPublishData

      Assert.shouldEqual id1 id2
