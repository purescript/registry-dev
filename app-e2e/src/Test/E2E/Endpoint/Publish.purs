module Test.E2E.Endpoint.Publish (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Registry.API.V1 (Job(..))
import Registry.API.V1 as V1
import Registry.Manifest (Manifest(..))
import Registry.Metadata (Metadata(..))
import Registry.Sha256 as Sha256
import Registry.Test.Assert as Assert
import Registry.Version as Version
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env (E2ESpec)
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as Fixtures
import Test.E2E.Support.WireMock as WireMock
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.describe "Publish workflow" do
    Spec.it "can publish effect@4.0.0 and verify all state changes" do
      { jobId } <- Client.publish Fixtures.effectPublishData
      job <- Env.pollJobOrFail jobId
      Assert.shouldSatisfy (V1.jobInfo job).finishedAt isJust

      uploadOccurred <- Env.hasStorageUpload Fixtures.effect
      unless uploadOccurred do
        storageRequests <- WireMock.getStorageRequests
        WireMock.failWithRequests "Expected S3 PUT for effect/4.0.0.tar.gz" storageRequests

      Metadata metadata <- Env.readMetadata Fixtures.effect.name
      case Map.lookup Fixtures.effect.version metadata.published of
        Nothing -> Assert.fail $ "Expected version " <> Version.print Fixtures.effect.version <> " in metadata published versions"
        Just publishedMeta -> do
          Assert.shouldSatisfy (Sha256.print publishedMeta.hash) (not <<< String.null)

      manifestEntries <- Env.readManifestIndexEntry Fixtures.effect.name
      let hasVersion = Array.any (\(Manifest m) -> m.version == Fixtures.effect.version) manifestEntries
      unless hasVersion do
        Assert.fail $ "Expected version " <> Version.print Fixtures.effect.version <> " in manifest index"

      Env.waitForAllMatrixJobs Fixtures.effect

      -- Collect the compilers from the matrix jobs that ran for this package
      allJobs <- Client.getJobsWith Client.IncludeCompleted
      let
        matrixCompilers = Array.mapMaybe
          ( case _ of
              MatrixJob { packageName, packageVersion, compilerVersion } ->
                if packageName == Fixtures.effect.name && packageVersion == Fixtures.effect.version then Just compilerVersion
                else Nothing
              _ -> Nothing
          )
          allJobs
        -- The expected compilers are: the publish compiler + all matrix job compilers
        expectedCompilers = Set.fromFoldable $ maybe matrixCompilers (\c -> Array.cons c matrixCompilers) Fixtures.effectPublishData.compiler

      Metadata metadataAfter <- Env.readMetadata Fixtures.effect.name
      case Map.lookup Fixtures.effect.version metadataAfter.published of
        Nothing -> Assert.fail "Version missing after matrix jobs"
        Just publishedMetaAfter -> do
          let actualCompilers = Set.fromFoldable $ NEA.toArray publishedMetaAfter.compilers
          Assert.shouldEqual actualCompilers expectedCompilers

  Spec.describe "Publish state machine" do
    Spec.it "returns same jobId for duplicate publish requests" do
      { jobId: id1 } <- Client.publish Fixtures.effectPublishData
      _ <- Env.pollJobOrFail id1
      { jobId: id2 } <- Client.publish Fixtures.effectPublishData
      Assert.shouldEqual id1 id2
