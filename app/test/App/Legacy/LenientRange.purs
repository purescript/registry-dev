module Test.Registry.App.Legacy.LenientRange (spec) where

import Registry.App.Prelude

import Data.Either as Either
import Registry.App.Legacy.LenientRange as LenientRange
import Registry.Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Parses malformed ranges with fixable errors" do
    for_ fixable \{ raw, range } ->
      Spec.it raw do
        let parsed = LenientRange.parse raw
        range `Assert.shouldEqualRight` map LenientRange.print parsed

  Spec.describe "Fails to parse unfixable malformed ranges" do
    for_ unfixable \{ raw, label } ->
      Spec.it label do
        let parsed = LenientRange.parse raw
        parsed `Assert.shouldSatisfy` Either.isLeft

fixable :: Array { raw :: String, range :: String }
fixable =
  [ { raw: "1", range: ">=1.0.0 <2.0.0" }
  , { raw: "2.3", range: ">=2.3.0 <2.4.0" }
  , { raw: "1 - 2", range: ">=1.0.0 <3.0.0" }
  , { raw: "1.0 - 2.0", range: ">=1.0.0 <2.1.0" }
  , { raw: "1.x.x", range: ">=1.0.0 <2.0.0" }
  , { raw: "1.2.x", range: ">=1.2.0 <1.3.0" }
  , { raw: "1.*.*", range: ">=1.0.0 <2.0.0" }
  , { raw: ">=\t1.0.0 <2.0.0", range: ">=1.0.0 <2.0.0" }
  , { raw: "~1", range: ">=1.0.0 <2.0.0" }
  , { raw: "~> 1", range: ">=1.0.0 <2.0.0" }
  , { raw: "~1.0", range: ">=1.0.0 <1.1.0" }
  , { raw: "~>3.2.1", range: ">=3.2.1 <3.3.0" }
  , { raw: "^ 1", range: ">=1.0.0 <2.0.0" }
  , { raw: "^0.1", range: ">=0.1.0 <0.2.0" }
  , { raw: "^1.0", range: ">=1.0.0 <2.0.0" }
  , { raw: "^1.2", range: ">=1.2.0 <2.0.0" }
  , { raw: "^0.0.1", range: ">=0.0.1 <0.0.2" }
  , { raw: ">=01.02.03 <01.02.10", range: ">=1.2.3 <1.2.10" }
  , { raw: "1.2.3 - 3.4", range: ">=1.2.3 <3.5.0" }
  , { raw: "1.2 - 3.4", range: ">=1.2.0 <3.5.0" }
  , { raw: "1.0.0 - 2.0.0", range: ">=1.0.0 <2.0.1" }
  , { raw: "1.0.0", range: ">=1.0.0 <1.0.1" }
  , { raw: ">1.0.0 <2.0.0", range: ">=1.0.1 <2.0.0" }
  , { raw: "<1.0.0", range: ">=0.0.0 <1.0.0" }
  , { raw: "<=1.0.0", range: ">=0.0.0 <1.0.1" }
  , { raw: "^0", range: ">=0.0.0 <1.0.0" }
  , { raw: ">01.02.03 <01.02.10", range: ">=1.2.4 <1.2.10" }
  , { raw: "^0.0.1-beta", range: ">=0.0.1 <0.0.2" }
  , { raw: ">=1.0.0-rc.1 <1.0.0-rc.5", range: ">=1.0.0 <1.0.1" }
  , { raw: "0.0.1+build", range: ">=0.0.1 <0.0.2" }
  ]

unfixable :: Array { raw :: String, label :: String }
unfixable =
  [ { raw: "*", label: "Results in *" }
  , { raw: ">=*", label: "Results in *" }
  , { raw: ">1.0.0", label: "Unbounded upper range" }
  , { raw: ">=1.0.0", label: "Unbounded upper range" }
  ]
