module Test.Registry.App.Legacy.BoundWidening (spec) where

import Registry.App.Prelude

import Data.Map as Map
import Data.Set as Set
import Registry.App.Legacy.BoundWidening as BoundWidening
import Registry.Internal.Codec as Internal.Codec
import Registry.Range as Range
import Registry.Test.Utils as Utils
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec.Spec Unit
spec = do
  Spec.describe "widenRangeWithEvidence" do
    Spec.it "returns original range when evidence is within range" do
      let
        original = Utils.unsafeRange ">=1.0.0 <2.0.0"
        evidence = Set.fromFoldable [ Utils.unsafeVersion "1.5.0" ]
        result = BoundWidening.widenRangeWithEvidence original evidence
      Range.print result `Assert.shouldEqual` Range.print original

    Spec.it "widens to next major for X.Y.Z where X >= 1" do
      let
        original = Utils.unsafeRange ">=1.0.0 <2.0.0"
        evidence = Set.fromFoldable [ Utils.unsafeVersion "5.0.0" ]
        result = BoundWidening.widenRangeWithEvidence original evidence
        expected = Utils.unsafeRange ">=1.0.0 <6.0.0"
      Range.print result `Assert.shouldEqual` Range.print expected

    Spec.it "widens to next minor for 0.Y.Z where Y >= 1" do
      let
        original = Utils.unsafeRange ">=0.1.0 <0.2.0"
        evidence = Set.fromFoldable [ Utils.unsafeVersion "0.5.3" ]
        result = BoundWidening.widenRangeWithEvidence original evidence
        expected = Utils.unsafeRange ">=0.1.0 <0.6.0"
      Range.print result `Assert.shouldEqual` Range.print expected

    Spec.it "widens to next patch for 0.0.Z" do
      let
        original = Utils.unsafeRange ">=0.0.1 <0.0.2"
        evidence = Set.fromFoldable [ Utils.unsafeVersion "0.0.5" ]
        result = BoundWidening.widenRangeWithEvidence original evidence
        expected = Utils.unsafeRange ">=0.0.1 <0.0.6"
      Range.print result `Assert.shouldEqual` Range.print expected

    Spec.it "returns original range when evidence is empty" do
      let
        original = Utils.unsafeRange ">=1.0.0 <2.0.0"
        evidence = Set.empty
        result = BoundWidening.widenRangeWithEvidence original evidence
      Range.print result `Assert.shouldEqual` Range.print original

    Spec.it "uses maximum observed version for widening" do
      let
        original = Utils.unsafeRange ">=1.0.0 <2.0.0"
        evidence = Set.fromFoldable [ Utils.unsafeVersion "3.0.0", Utils.unsafeVersion "5.0.0", Utils.unsafeVersion "4.0.0" ]
        result = BoundWidening.widenRangeWithEvidence original evidence
        expected = Utils.unsafeRange ">=1.0.0 <6.0.0"
      Range.print result `Assert.shouldEqual` Range.print expected

    Spec.it "does not narrow range when evidence is lower than upper bound" do
      let
        original = Utils.unsafeRange ">=1.0.0 <10.0.0"
        evidence = Set.fromFoldable [ Utils.unsafeVersion "5.0.0" ]
        result = BoundWidening.widenRangeWithEvidence original evidence
      Range.print result `Assert.shouldEqual` Range.print original

  Spec.describe "widenManifestDependencies (same-major fallback)" do
    -- These tests verify that when a package version isn't in any package set,
    -- we fall back to evidence from other versions in the same major series.

    Spec.it "uses evidence from same major version when exact version not in set" do
      -- Scenario: arrays@7.0.0 is NOT in package set, but arrays@7.1.0 IS
      -- Evidence for 7.1.0 should be used to widen 7.0.0's deps
      let
        arrays = Utils.unsafePackageName "arrays"
        bifunctors = Utils.unsafePackageName "bifunctors"

        -- Evidence: arrays@7.1.0 compiled with bifunctors@6.0.0
        evidence = Map.singleton arrays
          $ Map.singleton (Utils.unsafeVersion "7.1.0")
          $ Map.singleton bifunctors (Set.singleton $ Utils.unsafeVersion "6.0.0")

        -- arrays@7.0.0 has strict bound on bifunctors
        originalDeps = Map.singleton bifunctors (Utils.unsafeRange ">=5.0.0 <6.0.0")

        -- Should widen to include 6.0.0
        result = BoundWidening.widenManifestDependencies evidence arrays (Utils.unsafeVersion "7.0.0") originalDeps
        expected = Map.singleton bifunctors (Utils.unsafeRange ">=5.0.0 <7.0.0")

      printDeps result `Assert.shouldEqual` printDeps expected

    Spec.it "prefers exact version evidence over same-major fallback" do
      -- Scenario: both arrays@7.0.0 and arrays@7.1.0 are in package sets
      -- Should use exact 7.0.0 evidence, not aggregated
      let
        arrays = Utils.unsafePackageName "arrays"
        bifunctors = Utils.unsafePackageName "bifunctors"

        -- Evidence: 7.0.0 compiled with bifunctors@5.0.0, 7.1.0 with bifunctors@6.0.0
        evidence = Map.singleton arrays $ Map.fromFoldable
          [ Tuple (Utils.unsafeVersion "7.0.0") (Map.singleton bifunctors (Set.singleton $ Utils.unsafeVersion "5.0.0"))
          , Tuple (Utils.unsafeVersion "7.1.0") (Map.singleton bifunctors (Set.singleton $ Utils.unsafeVersion "6.0.0"))
          ]

        -- Original deps
        originalDeps = Map.singleton bifunctors (Utils.unsafeRange ">=5.0.0 <6.0.0")

        -- Should use exact 7.0.0 evidence (5.0.0 is already in range, no widening)
        result = BoundWidening.widenManifestDependencies evidence arrays (Utils.unsafeVersion "7.0.0") originalDeps

      printDeps result `Assert.shouldEqual` printDeps originalDeps

    Spec.it "aggregates evidence from multiple same-major versions" do
      -- Scenario: arrays@7.0.0 not in set, but 7.1.0 and 7.2.0 are
      -- Should aggregate evidence from both
      let
        arrays = Utils.unsafePackageName "arrays"
        bifunctors = Utils.unsafePackageName "bifunctors"

        -- Evidence: 7.1.0 compiled with bifunctors@6.0.0, 7.2.0 with bifunctors@6.1.0
        evidence = Map.singleton arrays $ Map.fromFoldable
          [ Tuple (Utils.unsafeVersion "7.1.0") (Map.singleton bifunctors (Set.singleton $ Utils.unsafeVersion "6.0.0"))
          , Tuple (Utils.unsafeVersion "7.2.0") (Map.singleton bifunctors (Set.singleton $ Utils.unsafeVersion "6.1.0"))
          ]

        originalDeps = Map.singleton bifunctors (Utils.unsafeRange ">=5.0.0 <6.0.0")

        -- Should widen based on max observed (6.1.0 -> upper bound <7.0.0)
        result = BoundWidening.widenManifestDependencies evidence arrays (Utils.unsafeVersion "7.0.0") originalDeps
        expected = Map.singleton bifunctors (Utils.unsafeRange ">=5.0.0 <7.0.0")

      printDeps result `Assert.shouldEqual` printDeps expected

    Spec.it "does not use evidence from different major version" do
      -- Scenario: arrays@7.0.0 not in set, but arrays@6.0.0 IS
      -- Should NOT use 6.x evidence for 7.x
      let
        arrays = Utils.unsafePackageName "arrays"
        bifunctors = Utils.unsafePackageName "bifunctors"

        -- Evidence: only arrays@6.0.0 compiled with bifunctors@6.0.0
        evidence = Map.singleton arrays
          $ Map.singleton (Utils.unsafeVersion "6.0.0")
          $ Map.singleton bifunctors (Set.singleton $ Utils.unsafeVersion "6.0.0")

        originalDeps = Map.singleton bifunctors (Utils.unsafeRange ">=5.0.0 <6.0.0")

        -- Should NOT widen (different major version)
        result = BoundWidening.widenManifestDependencies evidence arrays (Utils.unsafeVersion "7.0.0") originalDeps

      printDeps result `Assert.shouldEqual` printDeps originalDeps

    Spec.it "treats 0.x versions specially (0.1.x and 0.2.x are different series)" do
      -- For 0.x versions, minor is effectively the major version
      -- So 0.1.0 should not use evidence from 0.2.0
      let
        pkg = Utils.unsafePackageName "some-pkg"
        dep = Utils.unsafePackageName "some-dep"

        -- Evidence: only 0.2.0 in package set
        evidence = Map.singleton pkg
          $ Map.singleton (Utils.unsafeVersion "0.2.0")
          $ Map.singleton dep (Set.singleton $ Utils.unsafeVersion "3.0.0")

        originalDeps = Map.singleton dep (Utils.unsafeRange ">=1.0.0 <2.0.0")

        -- Should NOT widen (0.1.x vs 0.2.x are different series)
        result = BoundWidening.widenManifestDependencies evidence pkg (Utils.unsafeVersion "0.1.0") originalDeps

      printDeps result `Assert.shouldEqual` printDeps originalDeps

    Spec.it "uses evidence within same 0.x.y series" do
      -- 0.1.0 should use evidence from 0.1.1
      let
        pkg = Utils.unsafePackageName "some-pkg"
        dep = Utils.unsafePackageName "some-dep"

        -- Evidence: 0.1.1 in package set
        evidence = Map.singleton pkg
          $ Map.singleton (Utils.unsafeVersion "0.1.1")
          $ Map.singleton dep (Set.singleton $ Utils.unsafeVersion "3.0.0")

        originalDeps = Map.singleton dep (Utils.unsafeRange ">=1.0.0 <2.0.0")

        -- Should widen (same 0.1.x series)
        result = BoundWidening.widenManifestDependencies evidence pkg (Utils.unsafeVersion "0.1.0") originalDeps
        expected = Map.singleton dep (Utils.unsafeRange ">=1.0.0 <4.0.0")

      printDeps result `Assert.shouldEqual` printDeps expected

-- Helper to print deps map for comparison (avoids needing Show instance for PackageName)
printDeps :: Map PackageName Range -> String
printDeps = printJson (Internal.Codec.packageMap Range.codec)
