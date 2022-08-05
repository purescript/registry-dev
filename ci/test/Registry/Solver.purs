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
      [ p "p1" /\ v "0.0.0" /\ Map.fromFoldable
          [ p "p2" /\ r ">=0.0.0 <1.0.0" ]
      , p "p2" /\ v "0.0.0" /\ Map.empty
      ]
  Spec.it "Solves" do
    let
      goals = Map.fromFoldable
        [ p "p1" /\ r ">=0.0.0 <1.0.0" ]
    solve index goals `Assert.shouldEqual` map Just Map.fromFoldable
      [ p "p1" /\ v "0.0.0"
      , p "p2" /\ v "0.0.0"
      ]
