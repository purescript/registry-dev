-- | End-to-end tests for multi-operation workflows.
-- |
-- | These tests verify complex scenarios involving multiple operations,
-- | specifically dependency state validation across publish/unpublish sequences.
module Test.E2E.Workflow (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Registry.API.V1 as V1
import Registry.Metadata (Metadata(..))
import Registry.Test.Assert as Assert
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env (E2ESpec)
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as Fixtures
import Test.E2E.Support.WireMock as WireMock
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.describe "Dependency and unpublish interactions" do
    Spec.it "publishing a package fails when its dependency was unpublished" do
      { jobId: effectJobId } <- Client.publish Fixtures.effectPublishData
      _ <- Env.pollJobOrFail effectJobId

      authData <- Env.signUnpublishOrFail Fixtures.effectUnpublishData
      { jobId: unpublishJobId } <- Client.unpublish authData
      _ <- Env.pollJobOrFail unpublishJobId

      deleteOccurred <- Env.hasStorageDelete Fixtures.effect
      unless deleteOccurred do
        Assert.fail "Expected tarball delete from S3 for effect@4.0.0"

      manifestExists <- Env.manifestIndexEntryExists Fixtures.effect
      when manifestExists do
        Assert.fail "Expected effect@4.0.0 to be removed from manifest index after unpublish"

      WireMock.clearStorageRequests

      { jobId: consoleJobId } <- Client.publish Fixtures.consolePublishData
      consoleJob <- Env.pollJobExpectFailure consoleJobId

      let
        logs = (V1.jobInfo consoleJob).logs
        logMessages = map _.message logs
        hasDependencyError = Array.any (String.contains (String.Pattern "Could not produce valid dependencies")) logMessages
      unless hasDependencyError do
        Assert.fail $ "Expected dependency resolution error, got:\n" <> String.joinWith "\n" logMessages

      consoleUploadOccurred <- Env.hasStorageUpload Fixtures.console
      when consoleUploadOccurred do
        Assert.fail "Expected no tarball upload for console@6.1.0 after failed publish"

    Spec.it "unpublishing a package fails when dependents exist in manifest index" do
      { jobId: effectJobId } <- Client.publish Fixtures.effectPublishData
      _ <- Env.pollJobOrFail effectJobId

      { jobId: consoleJobId } <- Client.publish Fixtures.consolePublishData
      _ <- Env.pollJobOrFail consoleJobId

      WireMock.clearStorageRequests

      authData <- Env.signUnpublishOrFail Fixtures.effectUnpublishData
      { jobId: unpublishJobId } <- Client.unpublish authData
      unpublishJob <- Env.pollJobExpectFailure unpublishJobId

      let
        logs = (V1.jobInfo unpublishJob).logs
        logMessages = map _.message logs
        hasDependencyError = Array.any (String.contains (String.Pattern "unsatisfied dependencies")) logMessages
      unless hasDependencyError do
        Assert.fail $ "Expected unsatisfied dependencies error, got:\n" <>
          String.joinWith "\n" logMessages

      deleteOccurred <- Env.hasStorageDelete Fixtures.effect
      when deleteOccurred do
        Assert.fail "Expected no tarball delete for effect@4.0.0 after failed unpublish"

      manifestExists <- Env.manifestIndexEntryExists Fixtures.effect
      unless manifestExists do
        Assert.fail "Expected effect@4.0.0 to still exist in manifest index after failed unpublish"

      Metadata effectMeta <- Env.readMetadata Fixtures.effect.name
      unless (isJust $ Map.lookup Fixtures.effect.version effectMeta.published) do
        Assert.fail "Expected effect@4.0.0 to still be in published metadata after failed unpublish"
