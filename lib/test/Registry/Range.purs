module Test.Registry.Range (spec) where

import Prelude

import Data.Either as Either
import Data.Foldable (for_)
import Registry.Range as Range
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Parses well-formed ranges" do
    for_ valid \range ->
      Spec.it range do
        let parsed = Range.parse range
        range `Assert.shouldEqualRight` map Range.print parsed

  Spec.describe "Fails to parse malformed ranges" do
    for_ invalid \{ range, label } ->
      Spec.it label do
        let parsed = Range.parse range
        parsed `Assert.shouldSatisfy` Either.isLeft

  Spec.describe "Range inclusion" do
    Spec.it "Lower bound satisifies" do
      Utils.unsafeVersion "1.0.1" `Assert.shouldSatisfy` Range.includes (Utils.unsafeRange ">=1.0.1 <2.0.1")

    Spec.it "Upper bound satisifies" do
      Utils.unsafeVersion "2.0.0" `Assert.shouldSatisfy` Range.includes (Utils.unsafeRange ">=1.0.1 <2.0.1")

    Spec.it "Out of bounds fails" do
      Utils.unsafeVersion "0.1.1" `Assert.shouldNotSatisfy` Range.includes (Utils.unsafeRange ">=1.0.1 <2.0.1")
      Utils.unsafeVersion "3.0.0" `Assert.shouldNotSatisfy` Range.includes (Utils.unsafeRange ">=1.0.1 <2.0.1")

valid :: Array String
valid =
  [ ">=0.0.1 <0.0.2"
  , ">=0.1.0 <1.0.0"
  , ">=1.0.0 <1.1.5"
  ]

invalid :: Array { range :: String, label :: String }
invalid =
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
