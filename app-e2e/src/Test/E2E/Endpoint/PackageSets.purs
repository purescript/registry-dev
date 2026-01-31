module Test.E2E.Endpoint.PackageSets (spec) where

import Registry.App.Prelude

import Control.Monad.Reader (ask)
import Effect.Aff as Aff
import Registry.API.V1 as V1
import Registry.Test.Assert as Assert
import Test.E2E.Support.Client as Client
import Test.E2E.Support.Env (E2ESpec)
import Test.E2E.Support.Env as Env
import Test.E2E.Support.Fixtures as Fixtures
import Test.Spec as Spec

spec :: E2ESpec
spec = do
  Spec.describe "Package Sets endpoint" do
    Spec.it "accepts unauthenticated add/upgrade requests" do
      -- First publish unsafe-coerce to create the tarball in storage
      { jobId: publishJobId } <- Client.publish Fixtures.unsafeCoercePublishData
      _ <- Env.pollJobOrFail publishJobId
      -- Now add it to the package set
      { jobId } <- Client.packageSets Fixtures.packageSetAddRequest
      job <- Env.pollJobOrFail jobId
      Assert.shouldSatisfy (V1.jobInfo job).finishedAt isJust

    Spec.it "rejects unauthenticated compiler change requests" do
      result <- Client.tryPackageSets Fixtures.packageSetCompilerChangeRequest
      case result of
        Left err -> do
          Assert.shouldSatisfy (Client.clientErrorStatus err) (_ == Just 400)
        Right _ ->
          Assert.fail "Expected 400 error for unauthenticated compiler change"

    Spec.it "rejects unauthenticated package removal requests" do
      result <- Client.tryPackageSets Fixtures.packageSetRemoveRequest
      case result of
        Left err -> do
          Assert.shouldSatisfy (Client.clientErrorStatus err) (_ == Just 400)
        Right _ ->
          Assert.fail "Expected 400 error for unauthenticated package removal"

    Spec.it "accepts authenticated compiler change requests" do
      { privateKey } <- ask
      case Fixtures.signPackageSet privateKey Fixtures.packageSetCompilerChangeRequest of
        Left err ->
          liftAff $ Aff.throwError $ Aff.error $ "Failed to sign request: " <> err
        Right signedRequest -> do
          { jobId } <- Client.packageSets signedRequest
          job <- Env.pollJobOrFail jobId
          Assert.shouldSatisfy (V1.jobInfo job).finishedAt isJust

    Spec.it "returns existing job for duplicate requests" do
      -- First publish unsafe-coerce so the package set request is valid
      { jobId: publishJobId } <- Client.publish Fixtures.unsafeCoercePublishData
      _ <- Env.pollJobOrFail publishJobId
      -- Now test that duplicate requests return the same job ID
      { jobId: firstJobId } <- Client.packageSets Fixtures.packageSetAddRequest
      { jobId: secondJobId } <- Client.packageSets Fixtures.packageSetAddRequest
      Assert.shouldEqual firstJobId secondJobId
