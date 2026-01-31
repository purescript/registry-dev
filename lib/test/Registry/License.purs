module Test.Registry.License (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.String as String
import Registry.License as License
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "Parses well-formed licenses" do
    let { fail } = Utils.partitionEithers $ map License.parse valid
    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed licenses names were not parsed correctly:"
        , Array.foldMap (append "\n  - ") fail
        ]

  Spec.it "Fails to parse malformed licenses" do
    let { success } = Utils.partitionEithers $ map License.parse invalid
    unless (Array.null success) do
      Assert.fail $ String.joinWith "\n"
        [ "Some malformed package names were not parsed correctly:"
        , Array.foldMap (append "\n  - " <<< License.print) success
        ]

  Spec.it "joinWith creates valid parseable license expressions" do
    let
      licenses = [ License.parse "MIT", License.parse "Apache-2.0" ]
      { fail, success } = Utils.partitionEithers licenses

    unless (Array.null fail) do
      Assert.fail "Failed to parse test licenses"

    let
      joined = License.joinWith License.And success
      reparsed = License.parse (License.print joined)

    case reparsed of
      Left err -> Assert.fail $ "joinWith created unparseable expression: " <> License.print joined <> " - Error: " <> err
      Right _ -> pure unit

  Spec.describe "extractIds" do
    Spec.it "extracts single license ID" do
      case License.parse "MIT" of
        Left err -> Assert.fail err
        Right license -> case License.extractIds license of
          Left err -> Assert.fail err
          Right ids -> Assert.shouldEqual [ "MIT" ] ids

    Spec.it "extracts IDs from AND expression" do
      case License.parse "MIT AND Apache-2.0" of
        Left err -> Assert.fail err
        Right license -> case License.extractIds license of
          Left err -> Assert.fail err
          Right ids -> do
            Assert.shouldContain ids "MIT"
            Assert.shouldContain ids "APACHE-2.0"

    Spec.it "extracts IDs from OR expression" do
      case License.parse "MIT OR BSD-3-Clause" of
        Left err -> Assert.fail err
        Right license -> case License.extractIds license of
          Left err -> Assert.fail err
          Right ids -> do
            Assert.shouldContain ids "MIT"
            Assert.shouldContain ids "BSD-3-CLAUSE"

    Spec.it "extracts IDs from nested expression" do
      case License.parse "MIT AND (Apache-2.0 OR BSD-3-Clause)" of
        Left err -> Assert.fail err
        Right license -> case License.extractIds license of
          Left err -> Assert.fail err
          Right ids -> do
            Assert.shouldContain ids "MIT"
            Assert.shouldContain ids "APACHE-2.0"
            Assert.shouldContain ids "BSD-3-CLAUSE"

    Spec.it "normalizes license IDs to uppercase" do
      case License.parse "mit" of
        Left err -> Assert.fail err
        Right license -> case License.extractIds license of
          Left err -> Assert.fail err
          Right ids -> Assert.shouldEqual [ "MIT" ] ids

valid :: Array String
valid =
  [ "MIT"
  , "BSD-3-Clause"
  , "CC-BY-1.0"
  , "APACHE-2.0"
  , "LGPL-2.1-only"

  -- deprecated licenses are acceptable
  , "GPL-3.0"
  , "AGPL-1.0"

  -- conjunctions are understood
  , "LGPL-2.1 OR BSD-3-CLAUSE AND MIT"
  , "MIT AND (LGPL-2.1+ AND BSD-3-CLAUSE)"

  -- exceptions are understood
  , "GPS-3.0 WITH GPL-3.0-linking-exception"
  ]

invalid :: Array String
invalid =
  [ "Apache"
  , "Apache-2"
  , "Apache 2"
  , "BSD-3"
  , "MIT AND BSD-3"
  , "MIT AN BSD-3-Clause"
  ]
