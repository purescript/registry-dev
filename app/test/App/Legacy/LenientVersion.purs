module Test.Registry.App.Legacy.LenientVersion (spec) where

import Registry.App.Prelude

import Data.Either as Either
import Registry.App.Legacy.LenientVersion as LenientVersion
import Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Parses malformed versions with fixable errors" do
    for_ fixable \{ raw, version } ->
      Spec.it raw do
        let parsed = LenientVersion.parse raw
        version `Assert.shouldEqualRight` map LenientVersion.print parsed

  Spec.describe "Fails to parse unfixable malformed versions" do
    for_ unfixable \{ raw, label } ->
      Spec.it label do
        let parsed = LenientVersion.parse raw
        parsed `Assert.shouldSatisfy` Either.isLeft

fixable :: Array { raw :: String, version :: String }
fixable =
  [ { raw: "  1.0.0", version: "1.0.0" } -- leading spaces
  , { raw: "01.02.03", version: "1.2.3" } -- leading zeros
  , { raw: "1.0.0   ", version: "1.0.0" } -- trailing spaces
  , { raw: "v1.0.0", version: "1.0.0" } -- prefix v
  ]

unfixable :: Array { raw :: String, label :: String }
unfixable =
  [ { raw: ".12.0", label: "Malformed" }
  , { raw: "12.0", label: "Too few places" }
  , { raw: "0.12.12.1", label: "Too many places" }
  , { raw: "1.0.1-rc.1", label: "Contains prerelease identiiers" }
  , { raw: "1.0.0+nightly", label: "Contains build metadata" }
  , { raw: "1.a.2", label: "Contains non-digit characters" }
  , { raw: "1.-1.2", label: "Contains non-digit characters" }
  , { raw: "2147483648.0.0", label: "Contains non-32-bit integers" }
  ]
