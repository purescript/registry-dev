module Test.Registry.Solver where

import Registry.Prelude

import Data.Map as Map
import Data.Tuple (uncurry)
import Registry.PackageName as PackageName
import Registry.Solver (solve)
import Registry.Version (ParseMode(..))
import Registry.Version as Version
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec.Spec Unit
spec = do
  let
    p = unsafeFromRight <<< PackageName.parse
    v = unsafeFromRight <<< Version.parseVersion Strict
    r = unsafeFromRight <<< Version.parseRange Strict
    index = Map.fromFoldableWith Map.union $ (map <<< map) (uncurry Map.singleton) $
      -- simple and prelude have corresponding versions 0.0.0 and 1.0.0
      [ p "simple" /\ v "0.0.0" /\ Map.fromFoldable
          [ p "prelude" /\ r ">=0.0.0 <1.0.0" ]
      , p "simple" /\ v "1.0.0" /\ Map.fromFoldable
          [ p "prelude" /\ r ">=1.0.0 <2.0.0" ]
      , p "prelude" /\ v "0.0.0" /\ Map.empty
      , p "prelude" /\ v "1.0.0" /\ Map.empty
      -- not-updated only has version 0.0.0 depending on simple 0.0.0
      , p "not-updated" /\ v "0.0.0" /\ Map.fromFoldable
          [ p "simple" /\ r ">=0.0.0 <1.0.0" ]
      -- packages that are broken and fixed at different versions
      , p "broken-fixed" /\ v "0.0.0" /\ Map.fromFoldable
          [ p "does-not-exist" /\ r ">=0.0.0 <1.0.0" ]
      , p "broken-fixed" /\ v "1.0.0" /\ Map.fromFoldable
          [ p "prelude" /\ r ">=0.0.0 <1.0.0" ]
      , p "fixed-broken" /\ v "0.0.0" /\ Map.fromFoldable
          [ p "prelude" /\ r ">=0.0.0 <1.0.0" ]
      , p "fixed-broken" /\ v "1.0.0" /\ Map.fromFoldable
          [ p "does-not-exist" /\ r ">=0.0.0 <1.0.0" ]
      -- just broken
      , p "transitive-broken2" /\ v "0.0.0" /\ Map.fromFoldable
          [ p "broken-fixed" /\ r ">=0.0.0 <1.0.0" ]
      , p "transitive-broken1" /\ v "0.0.0" /\ Map.fromFoldable
          [ p "fixed-broken" /\ r ">=1.0.0 <2.0.0" ]
      ]
    testsucceeds goals result =
      solve index (Map.fromFoldable goals) `Assert.shouldEqual` Just (Map.fromFoldable result)
    testfails goals =
      solve index (Map.fromFoldable goals) `Assert.shouldEqual` Nothing
  Spec.it "Solves" do
    testsucceeds
      [ p "simple" /\ r ">=0.0.0 <1.0.0"
      ]
      [ p "simple" /\ v "0.0.0"
      , p "prelude" /\ v "0.0.0"
      ]
    testsucceeds
      [ p "simple" /\ r ">=0.0.0 <2.0.0"
      ]
      [ p "simple" /\ v "1.0.0"
      , p "prelude" /\ v "1.0.0"
      ]
    testsucceeds
      [ p "broken-fixed" /\ r ">=0.0.0 <2.0.0"
      ]
      [ p "broken-fixed" /\ v "1.0.0"
      , p "prelude" /\ v "0.0.0"
      ]
    testsucceeds
      [ p "broken-fixed" /\ r ">=1.0.0 <2.0.0"
      ]
      [ p "broken-fixed" /\ v "1.0.0"
      , p "prelude" /\ v "0.0.0"
      ]
    -- Loose bounds should find the older fixed version
    testsucceeds
      [ p "fixed-broken" /\ r ">=0.0.0 <2.0.0"
      ]
      [ p "fixed-broken" /\ v "0.0.0"
      , p "prelude" /\ v "0.0.0"
      ]
    testsucceeds
      [ p "fixed-broken" /\ r ">=0.0.0 <1.0.0"
      ]
      [ p "fixed-broken" /\ v "0.0.0"
      , p "prelude" /\ v "0.0.0"
      ]
    -- Test that tight bounds on simple/prelude forces the other to match
    testsucceeds
      [ p "simple" /\ r ">=0.0.0 <2.0.0" -- loose
      , p "prelude" /\ r ">=0.0.0 <1.0.0" -- tight
      ]
      [ p "simple" /\ v "0.0.0"
      , p "prelude" /\ v "0.0.0"
      ]
    testsucceeds
      [ p "simple" /\ r ">=0.0.0 <2.0.0" -- loose
      , p "prelude" /\ r ">=1.0.0 <2.0.0" -- tight
      ]
      [ p "simple" /\ v "1.0.0"
      , p "prelude" /\ v "1.0.0"
      ]
    testsucceeds
      [ p "prelude" /\ r ">=0.0.0 <2.0.0" -- loose
      , p "simple" /\ r ">=0.0.0 <1.0.0" -- tight
      ]
      [ p "simple" /\ v "0.0.0"
      , p "prelude" /\ v "0.0.0"
      ]
    testsucceeds
      [ p "prelude" /\ r ">=0.0.0 <2.0.0" -- loose
      , p "simple" /\ r ">=1.0.0 <2.0.0" -- tight
      ]
      [ p "simple" /\ v "1.0.0"
      , p "prelude" /\ v "1.0.0"
      ]
    testsucceeds
      [ p "prelude" /\ r ">=0.0.0 <2.0.0" -- loose
      , p "simple" /\ r ">=0.0.0 <2.0.0" -- loose
      ]
      [ p "simple" /\ v "1.0.0"
      , p "prelude" /\ v "1.0.0"
      ]
  Spec.it "Does not solve" do
    testfails
      [ p "does-not-exist" /\ r ">=0.0.0 <4.0.0" ]
    testfails
      [ p "broken-fixed" /\ r ">=0.0.0 <1.0.0" ]
    testfails
      [ p "fixed-broken" /\ r ">=1.0.0 <2.0.0" ]
    testfails
      [ p "transitive-broken1" /\ r ">=0.0.0 <1.0.0" ]
    testfails
      [ p "transitive-broken2" /\ r ">=0.0.0 <1.0.0" ]
    -- Incompatible versions
    testfails
      [ p "simple" /\ r ">=0.0.0 <1.0.0"
      , p "prelude" /\ r ">=1.0.0 <2.0.0"
      ]
    -- Updated prelude with unupdated package
    testfails
      [ p "not-updated" /\ r ">=0.0.0 <4.0.0"
      , p "prelude" /\ r ">=1.0.0 <2.0.0"
      ]
