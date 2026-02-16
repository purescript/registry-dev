module Test.Registry.License (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Registry.License as License
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "Parses canonical SPDX expressions" do
    let { fail } = Utils.partitionEithers $ map License.parse canonical
    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some canonical SPDX expressions were not parsed correctly:"
        , Array.foldMap (append "\n  - ") fail
        ]

  Spec.it "Fails to parse malformed SPDX expressions" do
    let { success } = Utils.partitionEithers $ map License.parse malformed
    unless (Array.null success) do
      Assert.fail $ String.joinWith "\n"
        [ "Some malformed SPDX expressions were parsed unexpectedly:"
        , Array.foldMap (append "\n  - " <<< License.print) success
        ]

  Spec.it "Strict parsing rejects deprecated SPDX identifiers" do
    let
      rejected =
        [ { input: "AGPL-3.0", expectedError: "AGPL-3.0-only" }
        , { input: "GPL-2.0-with-classpath-exception", expectedError: "unambiguous canonical replacement" }
        , { input: "GPL-2.0+", expectedError: "GPL-2.0-or-later" }
        , { input: "GFDL-1.3", expectedError: "unambiguous canonical replacement" }
        , { input: "GFDL-1.3+", expectedError: "GFDL-1.3-or-later" }
        ]

    for_ rejected \{ input, expectedError } ->
      case License.parse input of
        Right parsed ->
          Assert.fail $ "Expected strict parse to reject " <> input <> ", but parsed as " <> License.print parsed
        Left err ->
          unless (String.contains (Pattern expectedError) err) do
            Assert.fail $ "Expected parse error for " <> input <> " to mention " <> expectedError <> ", but got: " <> err

  Spec.it "Canonicalizes deterministic deprecated IDs in detected output" do
    let
      cases =
        [ { input: "AGPL-3.0", output: "AGPL-3.0-only" }
        , { input: "GPL-2.0+", output: "GPL-2.0-or-later" }
        , { input: "GFDL-1.3+", output: "GFDL-1.3-or-later" }
        , { input: "BSD-2-Clause-NetBSD", output: "BSD-2-Clause" }
        , { input: "StandardML-NJ", output: "SMLNJ" }
        ]

    for_ cases \{ input, output } ->
      case License.canonicalizeDetected input of
        Left err ->
          Assert.fail $ "Expected canonicalization to succeed for " <> input <> ", but failed with: " <> err
        Right canonicalized ->
          Assert.shouldEqual output canonicalized

  Spec.it "Fails to canonicalize ambiguous deprecated IDs in detected output" do
    for_ [ "BSD-2-Clause-FreeBSD", "GFDL-1.3", "Net-SNMP", "Nunit", "GPL-2.0-with-classpath-exception" ] \input ->
      case License.canonicalizeDetected input of
        Right canonicalized ->
          Assert.fail $ "Expected canonicalization to fail for " <> input <> ", but got " <> canonicalized
        Left err ->
          unless (String.contains (Pattern "unambiguous canonical replacement") err) do
            Assert.fail $ "Expected ambiguous canonicalization error for " <> input <> ", but got: " <> err

  Spec.it "Prints canonical SPDX expressions" do
    case License.parse "MIT AND (Apache-2.0 OR BSD-3-Clause)" of
      Left err ->
        Assert.fail err
      Right parsed ->
        Assert.shouldEqual "MIT AND (Apache-2.0 OR BSD-3-Clause)" (License.print parsed)

  Spec.it "joinWith creates valid parseable SPDX expressions" do
    let
      left = Utils.fromRight "Failed to parse MIT" (License.parse "MIT")
      right = Utils.fromRight "Failed to parse Apache-2.0" (License.parse "Apache-2.0")
      joined = License.joinWith License.And (Utils.unsafeNonEmptyArray [ left, right ])

    case License.parse (License.print joined) of
      Left err ->
        Assert.fail $ "joinWith created an unparseable expression: " <> License.print joined <> " - Error: " <> err
      Right _ ->
        pure unit

  Spec.describe "extractIds" do
    Spec.it "Extracts canonical uppercase IDs from parsed expressions" do
      case License.parse "MIT AND (Apache-2.0 OR BSD-3-Clause)" of
        Left err ->
          Assert.fail err
        Right parsed -> do
          let ids = License.extractIds parsed
          Assert.shouldContain ids "MIT"
          Assert.shouldContain ids "APACHE-2.0"
          Assert.shouldContain ids "BSD-3-CLAUSE"

canonical :: Array String
canonical =
  [ "MIT"
  , "BSD-3-Clause"
  , "CC-BY-1.0"
  , "Apache-2.0"
  , "LGPL-2.1-only"
  , "MIT AND Apache-2.0"
  , "MIT AND (LGPL-2.1-only OR BSD-3-Clause)"
  , "GPL-2.0-only WITH Classpath-exception-2.0"
  ]

malformed :: Array String
malformed =
  [ "Apache"
  , "Apache-2"
  , "Apache 2"
  , "BSD-3"
  , "MIT AND BSD-3"
  , "MIT AN BSD-3-Clause"
  , "MIT OR (Apache-2.0"
  ]
