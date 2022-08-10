module Test.Registry.Solver where

import Registry.Prelude

import Data.Map as Map
import Data.Set as Set
import Data.Tuple (uncurry)
import Registry.PackageName as PackageName
import Registry.Solver (SolverError(..), SolverPosition(..), solve)
import Registry.Version (ParseMode(..))
import Registry.Version as Version
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec.Spec Unit
spec = do
  let
    p = unsafeFromRight <<< PackageName.parse
    v = unsafeFromRight <<< Version.parseVersion Strict <<< (_ <> ".0.0") <<< show
    r l u = unsafeFromRight <<< Version.parseRange Strict
      $ ">="
      <> show l
      <> ".0.0 <"
      <> show u
      <> ".0.0"
    index = Map.fromFoldableWith Map.union $ (map <<< map) (uncurry Map.singleton) $
      -- simple and prelude have corresponding versions 0.0.0 and 1.0.0
      [ p "simple" /\ v 0 /\ Map.fromFoldable
          [ p "prelude" /\ r 0 1 ]
      , p "simple" /\ v 1 /\ Map.fromFoldable
          [ p "prelude" /\ r 1 2 ]
      , p "prelude" /\ v 0 /\ Map.empty
      , p "prelude" /\ v 1 /\ Map.empty
      -- not-updated only has version 0.0.0 depending on simple 0.0.0
      , p "not-updated" /\ v 0 /\ Map.fromFoldable
          [ p "simple" /\ r 0 1 ]
      -- packages that are broken and fixed at different versions
      , p "broken-fixed" /\ v 0 /\ Map.fromFoldable
          [ p "does-not-exist" /\ r 0 4 ]
      , p "broken-fixed" /\ v 1 /\ Map.fromFoldable
          [ p "prelude" /\ r 0 1 ]
      , p "fixed-broken" /\ v 0 /\ Map.fromFoldable
          [ p "prelude" /\ r 0 1 ]
      , p "fixed-broken" /\ v 1 /\ Map.fromFoldable
          [ p "does-not-exist" /\ r 0 5 ]
      -- just broken
      , p "transitive-broken2" /\ v 0 /\ Map.fromFoldable
          [ p "broken-fixed" /\ r 0 1 ]
      , p "transitive-broken1" /\ v 0 /\ Map.fromFoldable
          [ p "fixed-broken" /\ r 1 2 ]
      ]
    testsucceeds goals result =
      solve index (Map.fromFoldable goals) `Assert.shouldEqual` Right (Map.fromFoldable result)
    testfails goals error = do
      -- logShow error
      solve index (Map.fromFoldable goals) `Assert.shouldEqual` Left error
  Spec.it "Solves" do
    testsucceeds
      [ p "simple" /\ r 0 1
      ]
      [ p "simple" /\ v 0
      , p "prelude" /\ v 0
      ]
    testsucceeds
      [ p "simple" /\ r 0 2
      ]
      [ p "simple" /\ v 1
      , p "prelude" /\ v 1
      ]
    testsucceeds
      [ p "broken-fixed" /\ r 0 2
      ]
      [ p "broken-fixed" /\ v 1
      , p "prelude" /\ v 0
      ]
    testsucceeds
      [ p "broken-fixed" /\ r 1 2
      ]
      [ p "broken-fixed" /\ v 1
      , p "prelude" /\ v 0
      ]
    -- Loose bounds should find the older fixed version
    testsucceeds
      [ p "fixed-broken" /\ r 0 2
      ]
      [ p "fixed-broken" /\ v 0
      , p "prelude" /\ v 0
      ]
    testsucceeds
      [ p "fixed-broken" /\ r 0 1
      ]
      [ p "fixed-broken" /\ v 0
      , p "prelude" /\ v 0
      ]
    -- Test that tight bounds on simple/prelude forces the other to match
    testsucceeds
      [ p "simple" /\ r 0 2 -- loose
      , p "prelude" /\ r 0 1 -- tight
      ]
      [ p "simple" /\ v 0
      , p "prelude" /\ v 0
      ]
    testsucceeds
      [ p "simple" /\ r 0 2 -- loose
      , p "prelude" /\ r 1 2 -- tight
      ]
      [ p "simple" /\ v 1
      , p "prelude" /\ v 1
      ]
    testsucceeds
      [ p "prelude" /\ r 0 2 -- loose
      , p "simple" /\ r 0 1 -- tight
      ]
      [ p "simple" /\ v 0
      , p "prelude" /\ v 0
      ]
    testsucceeds
      [ p "prelude" /\ r 0 2 -- loose
      , p "simple" /\ r 1 2 -- tight
      ]
      [ p "simple" /\ v 1
      , p "prelude" /\ v 1
      ]
    testsucceeds
      [ p "prelude" /\ r 0 2 -- loose
      , p "simple" /\ r 0 2 -- loose
      ]
      [ p "simple" /\ v 1
      , p "prelude" /\ v 1
      ]
  Spec.it "Does not solve" do
    testfails
      [ p "prelude" /\ r 20 50 ]
      -- Package index contained no versions for prelude in the range >=20.0.0 <50.0.0 (existing versions: 0.0.0, 1.0.0)
      (NoVersionsInRange (p "prelude") (Set.fromFoldable [ v 0, v 1 ]) (r 20 50) SolveRoot)
    testfails
      [ p "does-not-exist" /\ r 0 4 ]
      -- Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none)
      (NoVersionsInRange (p "does-not-exist") Set.empty (r 0 4) SolveRoot)
    testfails
      [ p "broken-fixed" /\ r 0 1 ]
      -- Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none) while solving broken-fixed@0.0.0
      (NoVersionsInRange (p "does-not-exist") Set.empty (r 0 4) (Solving (p "broken-fixed") (v 0) SolveRoot))
    testfails
      [ p "fixed-broken" /\ r 1 2 ]
      -- Package index contained no versions for does-not-exist in the range >=0.0.0 <5.0.0 (existing versions: none) while solving fixed-broken@1.0.0
      (NoVersionsInRange (p "does-not-exist") Set.empty (r 0 5) (Solving (p "fixed-broken") (v 1) SolveRoot))
    testfails
      [ p "transitive-broken1" /\ r 0 1 ]
      -- Package index contained no versions for does-not-exist in the range >=0.0.0 <5.0.0 (existing versions: none) while solving fixed-broken@1.0.0 while solving transitive-broken1@0.0.0
      (NoVersionsInRange (p "does-not-exist") Set.empty (r 0 5) (Solving (p "fixed-broken") (v 1) (Solving (p "transitive-broken1") (v 0) SolveRoot)))
    testfails
      [ p "transitive-broken2" /\ r 0 1 ]
      -- Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none) while solving broken-fixed@0.0.0 while solving transitive-broken2@0.0.0
      (NoVersionsInRange (p "does-not-exist") Set.empty (r 0 4) (Solving (p "broken-fixed") (v 0) (Solving (p "transitive-broken2") (v 0) SolveRoot)))
    -- Incompatible versions
    testfails
      [ p "simple" /\ r 0 1
      , p "prelude" /\ r 1 2
      ]
      -- Committed to prelude@1.0.0 but the range >=0.0.0 <1.0.0 was also required while solving simple@0.0.0
      (VersionNotInRange (p "prelude") (v 1) (r 0 1) (Solving (p "simple") (v 0) SolveRoot))
    -- Updated prelude with unupdated package
    testfails
      [ p "not-updated" /\ r 0 4
      , p "prelude" /\ r 1 2
      ]
      -- Committed to prelude@1.0.0 but the range >=0.0.0 <1.0.0 was also required while solving simple@0.0.0 while solving not-updated@0.0.0
      (VersionNotInRange (p "prelude") (v 1) (r 0 1) (Solving (p "simple") (v 0) (Solving (p "not-updated") (v 0) SolveRoot)))
