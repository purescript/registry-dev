module Test.Registry.Operation.Validation where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Hours(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Registry.Manifest (Manifest(..))
import Registry.Metadata (Metadata(..))
import Registry.Operation.Validation (UnpublishError(..), forbiddenModules, getUnresolvedDependencies, validatePursModule, validateUnpublish)
import Registry.Test.Assert as Assert
import Registry.Test.Fixtures (defaultHash, defaultLocation)
import Registry.Test.Utils (fromJust, unsafeDateTime, unsafeManifest, unsafePackageName, unsafeVersion)
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "Dependency Resolution Checking" do
    checkDependencyResolution

  Spec.describe "Valid modules" do
    Spec.it "Accepts valid modules" do
      unit `Assert.shouldEqualRight` validatePursModule
        """
        module Mosh where
        """

      unit `Assert.shouldEqualRight` validatePursModule
        """
        {- module Main where -}
        module Mosh where
        """

      unit `Assert.shouldEqualRight` validatePursModule
        """
        -- module Main where
        module Mosh where
        """

      unit `Assert.shouldEqualRight` validatePursModule
        """
        -- module Main where {- module Main where {- module Main where -} module Main where -}
        module Mosh where

        -- The parser should not consider anything in the file beyond the header
        int zzzzz
        ab;;;
        module
        """

    Spec.it "Does not accept invalid modules" do
      for_ forbiddenModules \m ->
        when (Either.isRight (validatePursModule ("module " <> m <> " where"))) do
          Assert.fail $ "Accepted forbidden module name " <> m

  Spec.describe "Unpublish Validation" do
    let
      now = unsafeDateTime "2022-12-12T12:00:00.000Z"
      outOfRange = unsafeDateTime "2022-12-10T11:00:00.000Z"
      inRange = unsafeDateTime "2022-12-11T12:00:00.000Z"
      compilers = NonEmptyArray.singleton (unsafeVersion "0.13.0")

      publishedMetadata = { bytes: 100.0, hash: defaultHash, publishedTime: outOfRange, compilers, ref: Just "" }

      metadata = Metadata
        { location: defaultLocation
        , owners: Nothing
        , published: Map.fromFoldable [ Tuple (unsafeVersion "1.0.0") publishedMetadata ]
        , unpublished: Map.fromFoldable [ Tuple (unsafeVersion "2.0.0") { publishedTime: outOfRange, unpublishedTime: inRange, reason: "" } ]
        }

    Spec.it "Rejects when not yet published" do
      Assert.shouldEqual (validateUnpublish now (unsafeVersion "3.0.0") metadata) (Left NotPublished)

    Spec.it "Rejects when already unpublished" do
      Assert.shouldEqual (validateUnpublish now (unsafeVersion "2.0.0") metadata) (Left AlreadyUnpublished)

    Spec.it "Rejects when time limit is passed" do
      Assert.shouldEqual (validateUnpublish now (unsafeVersion "1.0.0") metadata) (Left (PastTimeLimit { limit: Hours 48.0, difference: Hours 49.0 }))

    Spec.it "Accepts valid input" do
      Assert.shouldEqual (validateUnpublish inRange (unsafeVersion "1.0.0") metadata) (Right publishedMetadata)

checkDependencyResolution :: Spec.Spec Unit
checkDependencyResolution = do
  Spec.it "Handles build plan with all dependencies resolved" do
    Assert.shouldEqual (getUnresolvedDependencies manifest exactBuildPlan) []

  Spec.it "Handles build plan with all dependencies resolved + extra" do
    Assert.shouldEqual (getUnresolvedDependencies manifest extraBuildPlan) []

  Spec.it "Handles build plan with resolution missing package" do
    Assert.shouldEqual (getUnresolvedDependencies manifest buildPlanMissingPackage) [ Left (packageTwoName /\ packageTwoRange) ]

  Spec.it "Handles build plan with resolution having package at wrong version" do
    Assert.shouldEqual (getUnresolvedDependencies manifest buildPlanWrongVersion) [ Right (packageTwoName /\ packageTwoRange /\ unsafeVersion "7.0.0") ]
  where
  manifest@(Manifest { dependencies }) =
    unsafeManifest "package" "1.0.0" [ Tuple "package-one" ">=2.0.0 <3.0.0", Tuple "package-two" ">=3.0.0 <4.0.0" ]

  packageTwoName = unsafePackageName "package-two"
  packageTwoRange = fromJust "Unable to lookup package name" $ Map.lookup packageTwoName dependencies

  exactBuildPlan = Map.fromFoldable
    [ Tuple (unsafePackageName "package-one") (unsafeVersion "2.0.0")
    , Tuple (unsafePackageName "package-two") (unsafeVersion "3.0.0")
    ]

  extraBuildPlan = Map.fromFoldable
    [ Tuple (unsafePackageName "package-one") (unsafeVersion "2.0.0")
    , Tuple (unsafePackageName "package-two") (unsafeVersion "3.0.0")
    , Tuple (unsafePackageName "package-three") (unsafeVersion "7.0.0")
    ]

  buildPlanMissingPackage = Map.fromFoldable
    [ Tuple (unsafePackageName "package-one") (unsafeVersion "2.0.0")
    , Tuple (unsafePackageName "package-three") (unsafeVersion "7.0.0")
    ]

  buildPlanWrongVersion = Map.fromFoldable
    [ Tuple (unsafePackageName "package-one") (unsafeVersion "2.0.0")
    , Tuple (unsafePackageName "package-two") (unsafeVersion "7.0.0")
    ]
