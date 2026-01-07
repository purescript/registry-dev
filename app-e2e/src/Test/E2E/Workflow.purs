-- | End-to-end tests for multi-operation workflows.
-- |
-- | These tests verify complex scenarios involving multiple operations:
-- | 1. Git state remains clean after multiple matrix jobs complete
-- | 2. Dependency state is validated correctly across publish/unpublish sequences
-- | 3. Job priority is respected when publish and matrix jobs overlap
module Test.E2E.Workflow (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.String as String
import Node.Path as Path
import Registry.API.V1 as V1
import Registry.Test.Assert as Assert
import Registry.Test.Fixtures as Fixtures
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as E2E.Fixtures
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "Concurrent git operations" do
    Spec.before_ Env.resetTestState do

      Spec.it "git repos remain clean after all matrix jobs complete" do
        config <- Env.getConfig

        { jobId: publishJobId } <- Env.expectRight "publish effect" =<< Client.publish config E2E.Fixtures.effectPublishData
        _ <- Env.pollJobOrFail config publishJobId
        Env.waitForAllMatrixJobs config Fixtures.effect.name Fixtures.effect.version

        -- Check scratch clones (not origin fixtures which have receive.denyCurrentBranch=ignore)
        stateDir <- Env.getStateDir
        let scratchRegistry = Path.concat [ stateDir, "scratch", "registry" ]
        let scratchRegistryIndex = Path.concat [ stateDir, "scratch", "registry-index" ]
        registryStatus <- Env.gitStatus scratchRegistry
        registryIndexStatus <- Env.gitStatus scratchRegistryIndex

        unless (Env.isCleanGitStatus registryStatus) do
          Assert.fail $ "registry repo has uncommitted changes:\n" <> registryStatus
        unless (Env.isCleanGitStatus registryIndexStatus) do
          Assert.fail $ "registry-index repo has uncommitted changes:\n" <> registryIndexStatus

  Spec.describe "Dependency and unpublish interactions" do
    Spec.before_ Env.resetTestState do

      Spec.it "publishing a package fails when its dependency was unpublished" do
        config <- Env.getConfig

        -- Publish then unpublish effect
        { jobId: effectJobId } <- Env.expectRight "publish effect" =<< Client.publish config E2E.Fixtures.effectPublishData
        _ <- Env.pollJobOrFail config effectJobId

        authData <- Env.signUnpublishOrFail E2E.Fixtures.effectUnpublishData
        { jobId: unpublishJobId } <- Env.expectRight "unpublish effect" =<< Client.unpublish config authData
        _ <- Env.pollJobOrFail config unpublishJobId

        -- Try to publish console (depends on effect ^4.0.0) - should fail
        { jobId: consoleJobId } <- Env.expectRight "submit console publish" =<< Client.publish config E2E.Fixtures.consolePublishData
        consoleJob <- Env.pollJobExpectFailure config consoleJobId

        -- Verify it failed due to dependency resolution (effect was unpublished)
        let
          logs = (V1.jobInfo consoleJob).logs
          logMessages = map _.message logs
          hasDependencyError = Array.any (String.contains (String.Pattern "Could not produce valid dependencies")) logMessages
        unless hasDependencyError do
          Assert.fail $ "Expected dependency resolution error, got:\n" <> String.joinWith "\n" logMessages

    Spec.before_ Env.resetTestState do

      Spec.it "unpublishing a package fails when dependents exist in manifest index" do
        config <- Env.getConfig

        -- Publish effect, then console (which depends on effect)
        { jobId: effectJobId } <- Env.expectRight "publish effect" =<< Client.publish config E2E.Fixtures.effectPublishData
        _ <- Env.pollJobOrFail config effectJobId

        { jobId: consoleJobId } <- Env.expectRight "publish console" =<< Client.publish config E2E.Fixtures.consolePublishData
        _ <- Env.pollJobOrFail config consoleJobId

        -- Try to unpublish effect - should fail because console depends on it
        authData <- Env.signUnpublishOrFail E2E.Fixtures.effectUnpublishData
        { jobId: unpublishJobId } <- Env.expectRight "submit unpublish" =<< Client.unpublish config authData
        unpublishJob <- Env.pollJobExpectFailure config unpublishJobId

        let
          logs = (V1.jobInfo unpublishJob).logs
          logMessages = map _.message logs
          hasDependencyError = Array.any (String.contains (String.Pattern "unsatisfied dependencies")) logMessages
        unless hasDependencyError do
          Assert.fail $ "Expected unsatisfied dependencies error, got:\n" <>
            String.joinWith "\n" logMessages

  Spec.describe "Job priority" do
    Spec.before_ Env.resetTestState do

      Spec.it "second publish job completes before first package's matrix jobs finish" do
        config <- Env.getConfig

        -- Publish effect to create matrix jobs
        { jobId: effectJobId } <- Env.expectRight "publish effect" =<< Client.publish config E2E.Fixtures.effectPublishData
        _ <- Env.pollJobOrFail config effectJobId

        -- Wait for matrix jobs to start running
        Env.waitForMatrixJobStart config Fixtures.effect.name Fixtures.effect.version

        -- Submit console publish while matrix jobs are in progress
        { jobId: consoleJobId } <- Env.expectRight "publish console" =<< Client.publish config E2E.Fixtures.consolePublishData
        consoleJob <- Env.pollJobOrFail config consoleJobId

        -- Verify console finished while some matrix jobs were still pending (proves priority)
        allJobs <- Env.expectRight "get jobs" =<< Client.getJobs config
        let effectMatrixJobs = Array.filter (Env.isMatrixJobFor Fixtures.effect.name Fixtures.effect.version) allJobs
        Assert.shouldSatisfy (Array.length effectMatrixJobs) (_ >= 1)

        let
          consoleFinishedAt = (V1.jobInfo consoleJob).finishedAt
          someMatrixPendingAtConsoleFinish = case consoleFinishedAt of
            Nothing -> false
            Just consoleFinishTime -> Array.any
              ( \j ->
                  let
                    info = V1.jobInfo j
                  in
                    case info.finishedAt of
                      Nothing -> true
                      Just matrixFinishTime -> matrixFinishTime > consoleFinishTime
              )
              effectMatrixJobs

        unless someMatrixPendingAtConsoleFinish do
          Assert.fail "Expected console publish to complete while some effect matrix jobs were still pending"

        -- Verify matrix jobs eventually complete (prevents false positive)
        Env.waitForAllMatrixJobs config Fixtures.effect.name Fixtures.effect.version
        finalJobs <- Env.expectRight "get final jobs" =<< Client.getJobs config
        let
          finalMatrixJobs = Array.filter (Env.isMatrixJobFor Fixtures.effect.name Fixtures.effect.version) finalJobs
          allFinished = Array.all (\j -> isJust (V1.jobInfo j).finishedAt) finalMatrixJobs
        unless allFinished do
          Assert.fail "Expected all matrix jobs to complete"
