module Test.Registry.Version
  ( testVersion
  , testRange
  ) where

import Registry.Prelude

import Data.Either as Either
import Registry.Version as Version
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

testVersion :: Spec.Spec Unit
testVersion = do
  Spec.describe "Invalid versions fail to parse" do
    for_ invalidVersions \{ version, label } ->
      Spec.it label do
        let parsed = Version.parseVersion version
        parsed `Assert.shouldSatisfy` Either.isLeft

  Spec.describe "Valid versions parse correctly and equal the parsed string" do
    for_ validVersions \version ->
      Spec.it version do
        let parsed = Version.parseVersion version
        map Version.printVersion parsed `Assert.shouldContain` version

validVersions :: Array String
validVersions =
  [ "0.0.0"
  , "0.0.1"
  , "0.1.0"
  , "1.0.0"
  , "1.2.3"
  , "1000.1111.1234"
  ]

invalidVersions :: Array { version :: String, label :: String }
invalidVersions =
  [ { version: ".12.0", label: "Malformed" }
  , { version: "12.0", label: "Too few places" }
  , { version: "0.12.12.1", label: "Too many places" }
  , { version: "v1.0.0", label: "Contains a prefix 'v'" }
  , { version: "0.000012.1", label: "Contains leading zeros" }
  , { version: "1.0.1-rc.1", label: "Contains prerelease identiiers" }
  , { version: "1.0.0+nightly", label: "Contains build metadata" }
  , { version: " 1.0.1", label: "Contains leading spaces" }
  , { version: "1.0.1 ", label: "Contains trailing spaces" }
  , { version: "1.a.2", label: "Contains non-digit characters" }
  , { version: "1.-1.2", label: "Contains non-digit characters" }
  , { version: "2147483648.0.0", label: "Contains non-32-bit integers" }
  ]

testRange :: Spec.Spec Unit
testRange = do
  Spec.describe "Invalid ranges fail to parse" do
    for_ invalidRanges \{ range, label } ->
      Spec.it label do
        let parsed = Version.parseRange range
        parsed `Assert.shouldSatisfy` Either.isLeft

  Spec.describe "Valid ranges parse correctly and equal the parsed string" do
    for_ validRanges \range ->
      Spec.it range do
        let parsed = Version.parseRange range
        map Version.printRange parsed `Assert.shouldContain` range

validRanges :: Array String
validRanges =
  [ ">=0.0.1 <0.0.2"
  , ">=0.1.0 <1.0.0"
  , ">=1.0.0 <1.1.5"
  ]

invalidRanges :: Array { range :: String, label :: String }
invalidRanges =
  [ { range: ">0.0.1 <0.0.2", label: "Uses a comparator other than >= on the lhs" }
  , { range: ">=0.0.1 <=0.0.2", label: "Uses a comparator other than < on the rhs" }
  , { range: ">=0.1.0 <0.1.0", label: "Left-hand version is not less than right-hand version" }
  , { range: "0.1.0 <= v < 0.2.0", label: "This isn't Elm" }
  , { range: " >=0.0.1 <0.0.2", label: "Contains leading spaces" }
  , { range: ">=0.0.1 <0.0.2 ", label: "Contains trailing spaces" }
  , { range: ">=0.0.1  <0.0.2", label: "Contains more than one space between versions" }
  , { range: ">=0.0.1.2 <0.0.2", label: "Contains an invalid version on the lhs" }
  , { range: ">=0.0.2 <0.2", label: "Contains an invalid version on the rhs" }
  , { range: ">=0.0.2", label: "Contains only the lhs version" }
  , { range: "<0.0.2", label: "Contains only the rhs version" }
  , { range: "0.0.2", label: "Contains no comparators" }
  ]
