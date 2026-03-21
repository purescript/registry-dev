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

  Spec.it "Parses and canonicalizes deterministic deprecated SPDX identifiers" do
    let
      cases =
        [ { input: "AGPL-3.0", output: "AGPL-3.0-only" }
        , { input: "AGPL-3.0+", output: "AGPL-3.0-or-later" }
        , { input: "eCos-2.0", output: "GPL-2.0-or-later WITH eCos-exception-2.0" }
        , { input: "LGPL-2.1", output: "LGPL-2.1-only" }
        , { input: "LGPL-3.0", output: "LGPL-3.0-only" }
        , { input: "LGPL-3.0+", output: "LGPL-3.0-or-later" }
        , { input: "GPL-3.0", output: "GPL-3.0-only" }
        , { input: "GPL-2.0-with-classpath-exception", output: "GPL-2.0-only WITH Classpath-exception-2.0" }
        , { input: "GPL-2.0+", output: "GPL-2.0-or-later" }
        , { input: "GPL-3.0 AND MIT", output: "GPL-3.0-only AND MIT" }
        , { input: "LGPL-2.1 AND LGPL-2.1-only", output: "LGPL-2.1-only AND LGPL-2.1-only" }
        , { input: "GFDL-1.3+", output: "GFDL-1.3-or-later" }
        , { input: "BSD-2-Clause-NetBSD", output: "BSD-2-Clause" }
        , { input: "StandardML-NJ", output: "SMLNJ" }
        , { input: "wxWindows", output: "LGPL-2.0-or-later WITH WxWindows-exception-3.1" }
        ]

    for_ cases \{ input, output } ->
      case License.parse input of
        Left err ->
          Assert.fail $ "Expected parse to succeed for " <> input <> ", but failed with: " <> err
        Right parsed ->
          Assert.shouldEqual output (License.print parsed)

  Spec.it "Canonical parsing rejects deprecated SPDX identifiers" do
    let
      rejected =
        [ { input: "AGPL-3.0", expectedError: "AGPL-3.0-only" }
        , { input: "AGPL-3.0+", expectedError: "AGPL-3.0-or-later" }
        , { input: "eCos-2.0", expectedError: "GPL-2.0-or-later WITH eCos-exception-2.0" }
        , { input: "LGPL-2.1", expectedError: "LGPL-2.1-only" }
        , { input: "LGPL-3.0", expectedError: "LGPL-3.0-only" }
        , { input: "LGPL-3.0+", expectedError: "LGPL-3.0-or-later" }
        , { input: "GPL-3.0", expectedError: "GPL-3.0-only" }
        , { input: "GPL-2.0-with-classpath-exception", expectedError: "GPL-2.0-only WITH Classpath-exception-2.0" }
        , { input: "GPL-2.0+", expectedError: "GPL-2.0-or-later" }
        , { input: "GPL-3.0 AND MIT", expectedError: "GPL-3.0-only" }
        , { input: "LGPL-2.1 AND LGPL-2.1-only", expectedError: "LGPL-2.1-only" }
        , { input: "GFDL-1.3", expectedError: "unambiguous canonical replacement" }
        , { input: "GFDL-1.3+", expectedError: "GFDL-1.3-or-later" }
        , { input: "wxWindows", expectedError: "LGPL-2.0-or-later WITH WxWindows-exception-3.1" }
        ]

    for_ rejected \{ input, expectedError } ->
      case License.parseCanonical input of
        Right parsed ->
          Assert.fail $ "Expected canonical parse to reject " <> input <> ", but parsed as " <> License.print parsed
        Left err ->
          unless (String.contains (Pattern expectedError) err) do
            Assert.fail $ "Expected parse error for " <> input <> " to mention " <> expectedError <> ", but got: " <> err

  Spec.it "Parses ambiguous deprecated SPDX identifiers without canonicalization" do
    let
      cases =
        [ { input: "GFDL-1.3", output: "GFDL-1.3" }
        , { input: "Net-SNMP", output: "Net-SNMP" }
        ]

    for_ cases \{ input, output } ->
      case License.parse input of
        Left err ->
          Assert.fail $ "Expected parse to succeed for " <> input <> ", but failed with: " <> err
        Right parsed ->
          Assert.shouldEqual output (License.print parsed)

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
