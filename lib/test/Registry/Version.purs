module Test.Registry.Version (spec) where

import Prelude

import Data.Either as Either
import Data.Foldable (for_)
import Registry.Version as Version
import Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Parses well-formed versions" do
    for_ valid \version ->
      Spec.it version do
        let parsed = Version.parse version
        version `Assert.shouldEqualRight` map Version.print parsed

  Spec.describe "Fails to parse malformed versions" do
    for_ invalid \{ version, label } ->
      Spec.it label do
        let parsed = Version.parse version
        parsed `Assert.shouldSatisfy` Either.isLeft

valid :: Array String
valid =
  [ "0.0.0"
  , "0.0.1"
  , "0.1.0"
  , "1.0.0"
  , "1.2.3"
  , "1000.1111.1234"
  ]

invalid :: Array { version :: String, label :: String }
invalid =
  [ { version: ".12.0", label: "Malformed" }
  , { version: "12.0", label: "Too few places" }
  , { version: "0.12.12.1", label: "Too many places" }
  , { version: "1.0.1-rc.1", label: "Contains prerelease identifiers" }
  , { version: "1.0.0+nightly", label: "Contains build metadata" }
  , { version: "1.a.2", label: "Contains non-digit characters" }
  , { version: "1.-1.2", label: "Contains non-digit characters" }
  , { version: "2147483648.0.0", label: "Contains non-32-bit integers" }
  , { version: "v1.0.0", label: "Contains a prefix 'v'" }
  , { version: "0.000012.1", label: "Contains leading zeros" }
  , { version: " 1.0.1", label: "Contains leading spaces" }
  , { version: "1.0.1 ", label: "Contains trailing spaces" }
  ]
