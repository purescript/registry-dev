module Test.Main where

import Registry.Prelude

import Data.Identity (Identity)
import Effect.Aff as Aff
import GitHub (IssueNumber(..))
import Partial.Unsafe (unsafeCrashWith)
import Registry.API as API
import Registry.PackageName as PackageName
import SPDX as SPDX
import Registry.Schema (Operation(..), Repo(..))
import SemVer as SemVer
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
      Spec.describe "SemVer" semVer

goodPackageName :: Spec
goodPackageName = do
  let parseName str res = Spec.it str do
        (PackageName.print <$> PackageName.parse str) `Assert.shouldEqual` (Right res)

  parseName "a" "a"
  parseName "some-dash" "some-dash"

badPackageName :: Spec
badPackageName = do
  let failParse str err = Spec.it str do
        (PackageName.print <$> PackageName.parse str) `Assert.shouldSatisfy` case _ of
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
      (SPDX.print <$> SPDX.parse str) `Assert.shouldSatisfy` isRight

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
    invalid str suggestion = "Invalid SPDX identifier: " <> str <> "." <> case suggestion of
      Nothing -> ""
      Just s -> " Did you mean " <> s <> "?"
    parseLicense str suggestion = Spec.it str do
      (SPDX.print <$> SPDX.parse str) `Assert.shouldSatisfy` case _ of
        Right _ -> false
        Left err -> err == invalid str suggestion

  -- common mistakes
  parseLicense "Apache" (Just "Apache-1.0")
  parseLicense "Apache-2" (Just "Apache-2.0")
  parseLicense "Apache 2" (Just "Apache-2.0")
  parseLicense "BSD-3" (Just "BSD-3-Clause")
  parseLicense "MIT AND BSD-3" Nothing

decodeEventsToOps :: Spec
decodeEventsToOps = do
  Spec.it "decodes an Update operation" do
    let
      issueNumber = IssueNumber 43
      operation = Update
        { packageName: fromRight' (\_ -> unsafeCrashWith "Expected Right") (PackageName.parse "something")
        , updateRef: "v1.2.3"
        , fromBower: false
        }

    res <- API.readOperation "test/fixtures/issue_comment.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber operation

  Spec.it "decodes an Addition operation" do
    let
      issueNumber = IssueNumber 149
      operation = Addition
        { packageName: fromRight' (\_ -> unsafeCrashWith "Expected Right") (PackageName.parse "prelude")
        , newRef: "v5.0.0"
        , fromBower: true
        , addToPackageSet: true
        , newPackageLocation: GitHub { subdir: Nothing, owner: "purescript", repo: "purescript-prelude" }
        }

    res <- API.readOperation "test/fixtures/issue_created.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber operation

semVer :: Spec
semVer = do
  let parseSemVer str = Spec.it ("Parse SemVer " <> str) do
        (SemVer.printSemVer <$> SemVer.parseSemVer str) `Assert.shouldSatisfy` isJust

  parseSemVer "v1.2.3"
  parseSemVer "1.2.3-rc2"
  parseSemVer "0.1.2+r2"

  let parseRange range expected = Spec.it ("Parse Range " <> show range <> " into " <> show expected) do
        (SemVer.printRange <$> SemVer.parseRange range) `Assert.shouldSatisfy` case _ of
          Just parsed -> parsed == expected
          Nothing -> false

  parseRange "^1.3.4" ">=1.3.4 <2.0.0-0"
