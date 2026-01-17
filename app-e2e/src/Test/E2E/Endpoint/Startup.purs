-- | E2E tests for server startup behavior (non-scheduler).
-- |
-- | - checkIfNewCompiler: Detects new compiler and enqueues matrix jobs
-- |
-- | IMPORTANT: These tests must run BEFORE resetTestState is called, since
-- | the jobs are created at server startup and would be cleared.
module Test.E2E.Endpoint.Startup (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Registry.API.V1 (Job(..))
import Registry.PackageName as PackageName
import Registry.Test.Assert as Assert
import Registry.Version as Version
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env (E2ESpec)
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.describe "checkIfNewCompiler" do
    Spec.it "enqueues matrix jobs for packages with no dependencies when new compiler detected" do
      -- The test env has compilers 0.15.10 and 0.15.11 available.
      -- prelude@6.0.1 fixture only has compiler 0.15.10 in metadata.
      -- So 0.15.11 should be detected as "new" at startup, triggering
      -- matrix jobs for packages with no dependencies.
      jobs <- Client.getJobs
      let
        isNewCompilerMatrixJob :: Job -> Boolean
        isNewCompilerMatrixJob = case _ of
          MatrixJob { compilerVersion } ->
            compilerVersion == unsafeFromRight (Version.parse "0.15.11")
          _ -> false

        matrixJobs = Array.filter isNewCompilerMatrixJob jobs

        -- Get package names from matrix jobs
        matrixPackages = Array.mapMaybe
          ( \j -> case j of
              MatrixJob { packageName } -> Just packageName
              _ -> Nothing
          )
          matrixJobs

      -- Should have matrix jobs for packages with no dependencies
      -- prelude has no dependencies, so it should get a matrix job
      let preludeName = unsafeFromRight (PackageName.parse "prelude")
      unless (Array.elem preludeName matrixPackages) do
        Assert.fail $ "Expected matrix job for prelude with compiler 0.15.11, found: "
          <> show (Array.length matrixJobs)
          <> " matrix jobs for packages: "
          <> show (map PackageName.print matrixPackages)
