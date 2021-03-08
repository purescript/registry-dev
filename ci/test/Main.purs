module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Registry.API as API
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.StringParser as Parse

type Spec = Spec.SpecT Aff Unit Identity Unit


main :: Effect Unit
main = Aff.launchAff_ $ runSpec [consoleReporter] do
  Spec.describe "API" do
    Spec.describe "Checks" do
      Spec.describe "Good package names" goodPackageName
      Spec.describe "Bad package names" badPackageName

goodPackageName :: Spec
goodPackageName = do
  let parseName str res = Spec.it str do
        (API.parsePackageName str) `Assert.shouldEqual` (Right res)

  parseName "a" "a"
  parseName "some-dash" "some-dash"


badPackageName :: Spec
badPackageName = do
  let failParse str err = Spec.it str do
        (API.parsePackageName str) `Assert.shouldSatisfy` case _ of
          Right _ -> false
          Left { error } -> error == err
  let startErr = "Package name should start with a lower case char or a digit"
  let midErr = "Package name can contain lower case chars, digits and non-consecutive dashes"
  let endErr = "Package name should end with a lower case char or digit"
  let manyDashes = "Package names cannot contain consecutive dashes"

  failParse "-a" startErr
  failParse "double--dash" manyDashes
  failParse "BIGLETTERS" startErr
  failParse "some space" midErr
  failParse "a-" endErr
  failParse "" startErr
  failParse "🍝" startErr
