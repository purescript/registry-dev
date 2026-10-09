module Test.E2E.Endpoint.Publish (spec) where

import Registry.App.Prelude

import Control.Monad.Reader (ask, runReaderT)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Effect.Aff as Aff
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
      { jobId, disposition: submissionDisposition } <- Client.publish Fixtures.effectPublishData
      Assert.shouldEqual submissionDisposition (Just V1.Created)
      job <- Env.pollJobOrFail jobId
      Assert.shouldSatisfy (V1.jobInfo job).finishedAt isJust
      case job of
        PublishJob details -> do
          Assert.shouldEqual details.disposition (Just V1.Published)
          Assert.shouldEqual details.error Nothing
        _ -> Assert.fail "Expected a publish job."

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

  Spec.describe "Publish with spago.dhall manifest" do
    Spec.it "can publish slug@3.0.0 which uses spago.dhall format" do
      { jobId } <- Client.publish Fixtures.slugPublishData
      job <- Env.pollJobOrFail jobId
      Assert.shouldSatisfy (V1.jobInfo job).finishedAt isJust

      uploadOccurred <- Env.hasStorageUpload Fixtures.slug
      unless uploadOccurred do
        storageRequests <- WireMock.getStorageRequests
        WireMock.failWithRequests "Expected S3 PUT for slug/3.0.0.tar.gz" storageRequests

      Metadata metadata <- Env.readMetadata Fixtures.slug.name
      case Map.lookup Fixtures.slug.version metadata.published of
        Nothing -> Assert.fail $ "Expected version " <> Version.print Fixtures.slug.version <> " in metadata published versions"
        Just publishedMeta -> do
          Assert.shouldSatisfy (Sha256.print publishedMeta.hash) (not <<< String.null)

      manifestEntries <- Env.readManifestIndexEntry Fixtures.slug.name
      let hasVersion = Array.any (\(Manifest m) -> m.version == Fixtures.slug.version) manifestEntries
      unless hasVersion do
        Assert.fail $ "Expected version " <> Version.print Fixtures.slug.version <> " in manifest index"

  Spec.describe "Publish state machine" do
    Spec.it "atomically deduplicates concurrent active and equivalent successful requests" do
      testEnv <- ask
      responses <- liftAff do
        fibers <- for (Array.range 1 5) \_ ->
          Aff.forkAff $ runReaderT (Client.publish Fixtures.effectPublishData) testEnv
        traverse Aff.joinFiber fibers

      case Array.uncons responses of
        Nothing -> Assert.fail "Expected publish responses."
        Just { head, tail } -> do
          unless (Array.all (_.jobId >>> (_ == head.jobId)) tail) do
            Assert.fail "Expected concurrent publish requests to return one job ID."
          let created = Array.filter (_.disposition >>> (_ == Just V1.Created)) responses
          let duplicateActive = Array.filter (_.disposition >>> (_ == Just V1.DuplicateActive)) responses
          Assert.shouldEqual (Array.length created) 1
          Assert.shouldEqual (Array.length duplicateActive) 4

          _ <- Env.pollJobOrFail head.jobId
          completedDuplicate <- Client.publish Fixtures.effectPublishData
          Assert.shouldEqual completedDuplicate.jobId head.jobId
          Assert.shouldEqual completedDuplicate.disposition (Just V1.AlreadyPublishedSubmission)

    Spec.it "reports an already-published package as idempotent success" do
      created <- Client.publish Fixtures.effectAlreadyPublishedData
      Assert.shouldEqual created.disposition (Just V1.Created)
      job <- Env.pollJobOrFail created.jobId
      case job of
        PublishJob details -> do
          Assert.shouldEqual details.disposition (Just V1.AlreadyPublished)
          Assert.shouldEqual details.error Nothing
        _ -> Assert.fail "Expected a publish job."

      duplicate <- Client.publish Fixtures.effectAlreadyPublishedData
      Assert.shouldEqual duplicate.jobId created.jobId
      Assert.shouldEqual duplicate.disposition (Just V1.AlreadyPublishedSubmission)
