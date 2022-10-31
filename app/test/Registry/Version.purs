module Test.Registry.Version
  ( testVersion
  , testRange
  ) where

import Registry.Prelude

import Data.Either as Either
import Registry.Version (ParseMode(..))
import Registry.Version as Version
import Test.Assert as Assert
import Test.Spec as Spec

testVersion :: Spec.Spec Unit
testVersion = do
  Spec.describe "Invalid strict versions fail to parse" do
    for_ invalidStrictVersions \{ version, label } ->
      Spec.it label do
        let parsed = Version.parseVersion Strict version
        parsed `Assert.shouldSatisfy` Either.isLeft

  Spec.describe "Valid strict versions parse correctly and equal the parsed string" do
    for_ validStrictVersions \version ->
      Spec.it version do
        let parsed = Version.parseVersion Strict version
        map Version.printVersion parsed `Assert.shouldContain` version
        map Version.rawVersion parsed `Assert.shouldContain` version

  Spec.describe "Invalid lenient versions fail to parse" do
    for_ invalidLenientVersions \{ version, label } ->
      Spec.it label do
        let parsed = Version.parseVersion Lenient version
        parsed `Assert.shouldSatisfy` Either.isLeft

  Spec.describe "Valid lenient versions parse correctly" do
    for_ validLenientVersions \{ raw, parsed } ->
      Spec.it raw do
        let parsedVersion = Version.parseVersion Lenient raw
        map Version.printVersion parsedVersion `Assert.shouldContain` parsed

  Spec.describe "Range inclusion works" do
    let unsafeVersion = unsafeFromRight <<< Version.parseVersion Version.Strict
    let unsafeRange = unsafeFromRight <<< Version.parseRange Version.Strict

    Spec.it "Lower bound satisifies" do
      unsafeVersion "1.0.1" `Assert.shouldSatisfy` Version.rangeIncludes (unsafeRange ">=1.0.1 <2.0.1")

    Spec.it "Upper bound satisifies" do
      unsafeVersion "2.0.0" `Assert.shouldSatisfy` Version.rangeIncludes (unsafeRange ">=1.0.1 <2.0.1")

    Spec.it "Out of bounds fails" do
      unsafeVersion "0.1.1" `Assert.shouldNotSatisfy` Version.rangeIncludes (unsafeRange ">=1.0.1 <2.0.1")
      unsafeVersion "3.0.0" `Assert.shouldNotSatisfy` Version.rangeIncludes (unsafeRange ">=1.0.1 <2.0.1")

validStrictVersions :: Array String
validStrictVersions =
  [ "0.0.0"
  , "0.0.1"
  , "0.1.0"
  , "1.0.0"
  , "1.2.3"
  , "1000.1111.1234"
  ]

invalidStrictVersions :: Array { version :: String, label :: String }
invalidStrictVersions = invalidLenientVersions <>
  [ { version: "v1.0.0", label: "Contains a prefix 'v'" }
  , { version: "0.000012.1", label: "Contains leading zeros" }
  , { version: " 1.0.1", label: "Contains leading spaces" }
  , { version: "1.0.1 ", label: "Contains trailing spaces" }
  ]

validLenientVersions :: Array { raw :: String, parsed :: String }
validLenientVersions =
  [ { raw: "  1.0.0", parsed: "1.0.0" }
  , { raw: "01.02.03", parsed: "1.2.3" }
  , { raw: "1.0.0   ", parsed: "1.0.0" }
  , { raw: "v1.0.0", parsed: "1.0.0" }
  ]

invalidLenientVersions :: Array { version :: String, label :: String }
invalidLenientVersions =
  [ { version: ".12.0", label: "Malformed" }
  , { version: "12.0", label: "Too few places" }
  , { version: "0.12.12.1", label: "Too many places" }
  , { version: "1.0.1-rc.1", label: "Contains prerelease identiiers" }
  , { version: "1.0.0+nightly", label: "Contains build metadata" }
  , { version: "1.a.2", label: "Contains non-digit characters" }
  , { version: "1.-1.2", label: "Contains non-digit characters" }
  , { version: "2147483648.0.0", label: "Contains non-32-bit integers" }
  ]

testRange :: Spec.Spec Unit
testRange = do
  Spec.describe "Invalid strict ranges fail to parse" do
    for_ invalidStrictRanges \{ range, label } ->
      Spec.it label do
        let parsed = Version.parseRange Strict range
        parsed `Assert.shouldSatisfy` Either.isLeft

  Spec.describe "Valid strict ranges parse correctly and equal the parsed string" do
    for_ validStrictRanges \range ->
      Spec.it range do
        let parsed = Version.parseRange Strict range
        map Version.printRange parsed `Assert.shouldContain` range
        map Version.rawRange parsed `Assert.shouldContain` range

  Spec.describe "Invalid lenient ranges fail to parse" do
    for_ invalidLenientRanges \{ range, label } ->
      Spec.it label do
        let parsed = Version.parseRange Lenient range
        parsed `Assert.shouldSatisfy` Either.isLeft

  Spec.describe "Valid lenient ranges parse correctly" do
    for_ validLenientRanges \{ raw, parsed } ->
      Spec.it raw do
        let parsedRange = Version.parseRange Lenient raw
        map Version.printRange parsedRange `Assert.shouldContain` parsed

validStrictRanges :: Array String
validStrictRanges =
  [ ">=0.0.1 <0.0.2"
  , ">=0.1.0 <1.0.0"
  , ">=1.0.0 <1.1.5"
  ]

invalidStrictRanges :: Array { range :: String, label :: String }
invalidStrictRanges =
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

validLenientRanges :: Array { raw :: String, parsed :: String }
validLenientRanges =
  [ { raw: "1", parsed: ">=1.0.0 <2.0.0" }
  , { raw: "2.3", parsed: ">=2.3.0 <2.4.0" }
  , { raw: "1 - 2", parsed: ">=1.0.0 <3.0.0" }
  , { raw: "1.0 - 2.0", parsed: ">=1.0.0 <2.1.0" }
  , { raw: "1.x.x", parsed: ">=1.0.0 <2.0.0" }
  , { raw: "1.2.x", parsed: ">=1.2.0 <1.3.0" }
  , { raw: "1.*.*", parsed: ">=1.0.0 <2.0.0" }
  , { raw: ">=\t1.0.0 <2.0.0", parsed: ">=1.0.0 <2.0.0" }
  , { raw: "~1", parsed: ">=1.0.0 <2.0.0" }
  , { raw: "~> 1", parsed: ">=1.0.0 <2.0.0" }
  , { raw: "~1.0", parsed: ">=1.0.0 <1.1.0" }
  , { raw: "~>3.2.1", parsed: ">=3.2.1 <3.3.0" }
  , { raw: "^ 1", parsed: ">=1.0.0 <2.0.0" }
  , { raw: "^0.1", parsed: ">=0.1.0 <0.2.0" }
  , { raw: "^1.0", parsed: ">=1.0.0 <2.0.0" }
  , { raw: "^1.2", parsed: ">=1.2.0 <2.0.0" }
  , { raw: "^0.0.1", parsed: ">=0.0.1 <0.0.2" }
  , { raw: ">=01.02.03 <01.02.10", parsed: ">=1.2.3 <1.2.10" }
  , { raw: "1.2.3 - 3.4", parsed: ">=1.2.3 <3.5.0" }
  , { raw: "1.2 - 3.4", parsed: ">=1.2.0 <3.5.0" }
  , { raw: "1.0.0 - 2.0.0", parsed: ">=1.0.0 <2.0.1" }
  , { raw: "1.0.0", parsed: ">=1.0.0 <1.0.1" }
  , { raw: ">1.0.0 <2.0.0", parsed: ">=1.0.1 <2.0.0" }
  , { raw: "<1.0.0", parsed: ">=0.0.0 <1.0.0" }
  , { raw: "<=1.0.0", parsed: ">=0.0.0 <1.0.1" }
  , { raw: "^0", parsed: ">=0.0.0 <1.0.0" }
  , { raw: ">01.02.03 <01.02.10", parsed: ">=1.2.4 <1.2.10" }
  , { raw: "^0.0.1-beta", parsed: ">=0.0.1 <0.0.2" }
  , { raw: ">=1.0.0-rc.1 <1.0.0-rc.5", parsed: ">=1.0.0 <1.0.1" }
  , { raw: "0.0.1+build", parsed: ">=0.0.1 <0.0.2" }
  ]

invalidLenientRanges :: Array { range :: String, label :: String }
invalidLenientRanges =
  [ { range: "*", label: "Results in *" }
  , { range: ">=*", label: "Results in *" }
  , { range: ">1.0.0", label: "Unbounded upper range" }
  , { range: ">=1.0.0", label: "Unbounded upper range" }
  ]
