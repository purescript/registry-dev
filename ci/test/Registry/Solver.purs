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
    -- For all these tests, we work with major versions only because we do not
    -- need to exercise the intricacies of the range relations, just the solver,
    -- which does not care about what versions are, just how they relate
    package = unsafeFromRight <<< PackageName.parse
    version = unsafeFromRight <<< Version.parseVersion Strict <<< (_ <> ".0.0") <<< show
    range l u = unsafeFromRight <<< Version.parseRange Strict
      $ ">="
      <> show l
      <> ".0.0 <"
      <> show u
      <> ".0.0"
    index = Map.fromFoldableWith Map.union $ (map <<< map) (uncurry Map.singleton) $
      -- simple and prelude have corresponding versions 0.0.0 and 1.0.0
      [ package "simple" /\ version 0 /\ Map.fromFoldable
          [ package "prelude" /\ range 0 1 ]
      , package "simple" /\ version 1 /\ Map.fromFoldable
          [ package "prelude" /\ range 1 2 ]
      , package "prelude" /\ version 0 /\ Map.empty
      , package "prelude" /\ version 1 /\ Map.empty
      -- not-updated only has version 0.0.0 depending on simple 0.0.0
      , package "not-updated" /\ version 0 /\ Map.fromFoldable
          [ package "simple" /\ range 0 1 ]
      -- packages that are broken and fixed at different versions
      , package "broken-fixed" /\ version 0 /\ Map.fromFoldable
          [ package "does-not-exist" /\ range 0 4 ]
      , package "broken-fixed" /\ version 1 /\ Map.fromFoldable
          [ package "prelude" /\ range 0 1 ]
      , package "fixed-broken" /\ version 0 /\ Map.fromFoldable
          [ package "prelude" /\ range 0 1 ]
      , package "fixed-broken" /\ version 1 /\ Map.fromFoldable
          [ package "does-not-exist" /\ range 0 5 ]
      , package "broken-broken" /\ version 0 /\ Map.fromFoldable
          [ package "does-not-exist" /\ range 0 5 ]
      , package "broken-broken" /\ version 1 /\ Map.fromFoldable
          [ package "does-not-exist" /\ range 0 5 ]
      -- just broken
      , package "transitive-broken2" /\ version 0 /\ Map.fromFoldable
          [ package "broken-fixed" /\ range 0 1 ]
      , package "transitive-broken1" /\ version 0 /\ Map.fromFoldable
          [ package "fixed-broken" /\ range 1 2 ]
      ]
    testsucceeds goals result =
      solve index (Map.fromFoldable goals) `Assert.shouldEqual` Right (Map.fromFoldable result)
    testfails goals error = do
      -- logShow error
      solve index (Map.fromFoldable goals) `Assert.shouldEqual` Left error
  Spec.it "Solves" do
    testsucceeds
      [ package "simple" /\ range 0 1
      ]
      [ package "simple" /\ version 0
      , package "prelude" /\ version 0
      ]
    testsucceeds
      [ package "simple" /\ range 0 2
      ]
      [ package "simple" /\ version 1
      , package "prelude" /\ version 1
      ]
    testsucceeds
      [ package "broken-fixed" /\ range 0 2
      ]
      [ package "broken-fixed" /\ version 1
      , package "prelude" /\ version 0
      ]
    testsucceeds
      [ package "broken-fixed" /\ range 1 2
      ]
      [ package "broken-fixed" /\ version 1
      , package "prelude" /\ version 0
      ]
    -- Loose bounds should find the older fixed version
    testsucceeds
      [ package "fixed-broken" /\ range 0 2
      ]
      [ package "fixed-broken" /\ version 0
      , package "prelude" /\ version 0
      ]
    testsucceeds
      [ package "fixed-broken" /\ range 0 1
      ]
      [ package "fixed-broken" /\ version 0
      , package "prelude" /\ version 0
      ]
    -- Test that tight bounds on simple/prelude forces the other to match
    testsucceeds
      [ package "simple" /\ range 0 2 -- loose
      , package "prelude" /\ range 0 1 -- tight
      ]
      [ package "simple" /\ version 0
      , package "prelude" /\ version 0
      ]
    testsucceeds
      [ package "simple" /\ range 0 2 -- loose
      , package "prelude" /\ range 1 2 -- tight
      ]
      [ package "simple" /\ version 1
      , package "prelude" /\ version 1
      ]
    testsucceeds
      [ package "prelude" /\ range 0 2 -- loose
      , package "simple" /\ range 0 1 -- tight
      ]
      [ package "simple" /\ version 0
      , package "prelude" /\ version 0
      ]
    testsucceeds
      [ package "prelude" /\ range 0 2 -- loose
      , package "simple" /\ range 1 2 -- tight
      ]
      [ package "simple" /\ version 1
      , package "prelude" /\ version 1
      ]
    testsucceeds
      [ package "prelude" /\ range 0 2 -- loose
      , package "simple" /\ range 0 2 -- loose
      ]
      [ package "simple" /\ version 1
      , package "prelude" /\ version 1
      ]
  Spec.it "Does not solve" do
    testfails
      [ package "prelude" /\ range 20 50 ]
      -- Package index contained no versions for prelude in the range >=20.0.0 <50.0.0 (existing versions: 0.0.0, 1.0.0)
      (pure $ NoVersionsInRange (package "prelude") (Set.fromFoldable [ version 0, version 1 ]) (range 20 50) SolveRoot)
    testfails
      [ package "does-not-exist" /\ range 0 4 ]
      -- Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none)
      (pure $ NoVersionsInRange (package "does-not-exist") Set.empty (range 0 4) SolveRoot)
    testfails
      [ package "broken-fixed" /\ range 0 1 ]
      -- Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none) while solving broken-fixed@0.0.0
      (pure $ NoVersionsInRange (package "does-not-exist") Set.empty (range 0 4) (Solving (package "broken-fixed") (version 0) SolveRoot))
    testfails
      [ package "fixed-broken" /\ range 1 2 ]
      -- Package index contained no versions for does-not-exist in the range >=0.0.0 <5.0.0 (existing versions: none) while solving fixed-broken@1.0.0
      (pure $ NoVersionsInRange (package "does-not-exist") Set.empty (range 0 5) (Solving (package "fixed-broken") (version 1) SolveRoot))
    testfails
      [ package "transitive-broken1" /\ range 0 1 ]
      -- Package index contained no versions for does-not-exist in the range >=0.0.0 <5.0.0 (existing versions: none) while solving fixed-broken@1.0.0 while solving transitive-broken1@0.0.0
      (pure $ NoVersionsInRange (package "does-not-exist") Set.empty (range 0 5) (Solving (package "fixed-broken") (version 1) (Solving (package "transitive-broken1") (version 0) SolveRoot)))
    testfails
      [ package "transitive-broken2" /\ range 0 1 ]
      -- Package index contained no versions for does-not-exist in the range >=0.0.0 <4.0.0 (existing versions: none) while solving broken-fixed@0.0.0 while solving transitive-broken2@0.0.0
      (pure $ NoVersionsInRange (package "does-not-exist") Set.empty (range 0 4) (Solving (package "broken-fixed") (version 0) (Solving (package "transitive-broken2") (version 0) SolveRoot)))
    -- Incompatible versions
    testfails
      [ package "simple" /\ range 0 1
      , package "prelude" /\ range 1 2
      ]
      -- Committed to prelude@1.0.0 but the range >=0.0.0 <1.0.0 was also required while solving simple@0.0.0
      (pure $ VersionNotInRange (package "prelude") (version 1) (range 0 1) (Solving (package "simple") (version 0) SolveRoot))
    -- Updated prelude with unupdated package
    testfails
      [ package "not-updated" /\ range 0 4
      , package "prelude" /\ range 1 2
      ]
      -- Committed to prelude@1.0.0 but the range >=0.0.0 <1.0.0 was also required while solving simple@0.0.0 while solving not-updated@0.0.0
      (pure $ VersionNotInRange (package "prelude") (version 1) (range 0 1) (Solving (package "simple") (version 0) (Solving (package "not-updated") (version 0) SolveRoot)))
    -- Updated prelude with unupdated package
    testfails
      [ package "not-updated" /\ range 0 4
      , package "prelude" /\ range 1 2
      ]
      -- Committed to prelude@1.0.0 but the range >=0.0.0 <1.0.0 was also required while solving simple@0.0.0 while solving not-updated@0.0.0
      (pure $ VersionNotInRange (package "prelude") (version 1) (range 0 1) (Solving (package "simple") (version 0) (Solving (package "not-updated") (version 0) SolveRoot)))
    -- Multiple errors, since broken-broken@1.0.0 and broken-broken@0.0.0 both fit but cannot be solved
    testfails
      [ package "broken-broken" /\ range 0 2
      ]
      $
        -- Note that the higher version will appear first!
        (pure (version 1) <|> pure (version 0))
      <#> \brokenBrokenVersion -> do
        NoVersionsInRange (package "does-not-exist") Set.empty (range 0 5) (Solving (package "broken-broken") brokenBrokenVersion SolveRoot)
