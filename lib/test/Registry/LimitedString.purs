module Test.Registry.LimitedString (spec) where

import Prelude

import Data.Either (Either, isLeft, isRight)
import Registry.LimitedString (LimitedString)
import Registry.LimitedString as LimitedString
import Registry.Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Parses strings up to the type-level limit" do
    (LimitedString.parse "abc" :: Either String (LimitedString 3)) `Assert.shouldSatisfy` isRight

  Spec.it "Rejects strings over the type-level limit" do
    (LimitedString.parse "abcd" :: Either String (LimitedString 3)) `Assert.shouldSatisfy` isLeft
