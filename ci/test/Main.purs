module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Registry.API as API
import Registry.SPDX as SPDX
import Registry.Schema (Operation(..), Repo(..))
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

type Spec = Spec.SpecT Aff Unit Identity Unit

main :: Effect Unit
main = Aff.launchAff_ $ runSpec [consoleReporter] do
  Spec.describe "API" do
    Spec.describe "Checks" do
      Spec.describe "Good package names" goodPackageName
      Spec.describe "Bad package names" badPackageName
      Spec.describe "Good SPDX licenses" goodSPDXLicense
      Spec.describe "Bad SPDX licenses" badSPDXLicense
      Spec.describe "Decode GitHub event to Operation" decodeEventsToOps

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

goodSPDXLicense :: Spec
goodSPDXLicense = do
  let
    parseLicense str = Spec.it str do
      (SPDX.isValidSPDXLicenseId str) `Assert.shouldEqual` true

  -- current licenses
  parseLicense "MIT"
  parseLicense "BSD-3-Clause"
  parseLicense "CC-BY-1.0"
  parseLicense "Apache-2.0"

  -- deprecated licenses
  parseLicense "GPL-3.0"
  parseLicense "AGPL-1.0"

  -- combinations
  parseLicense "LGPL-2.1 OR BSD-3-Clause AND MIT"
  parseLicense "MIT AND (LGPL-2.1+ AND BSD-3-Clause)"

  -- exceptions
  parseLicense "GPL-3.0 WITH GPL-3.0-linking-exception"

badSPDXLicense :: Spec
badSPDXLicense = do
  let
    parseLicense str = Spec.it str do
      (SPDX.isValidSPDXLicenseId str) `Assert.shouldEqual` false

  -- common mistakes
  parseLicense "Apache"
  parseLicense "Apache-2"
  parseLicense "Apache 2"
  parseLicense "BSD-3"

decodeEventsToOps :: Spec
decodeEventsToOps = do
  Spec.it "decodes an Update operation" do
    res <- API.readOperation "test/fixtures/issue_created.json"
    res `Assert.shouldEqual` API.DecodedOperation (Update { packageName: "something", updateRef: "v1.2.3", fromBower: false })

  Spec.it "decodes an Addition operation" do
    res <- API.readOperation "test/fixtures/issue_comment.json"
    res `Assert.shouldEqual` API.DecodedOperation (Addition { packageName: "something", newRef: "v1.2.3", fromBower: false, addToPackageSet: true, newPackageLocation: GitHub { subdir: Nothing, owner: "purescript", repo: "purescript-something" } })
