module Test.Registry.License (spec) where

import Prelude

import Data.Array as Array
import Data.String as String
import Registry.License as License
import Test.Spec as Spec
import Test.Utils as Utils
import Test.Utils.Assert as Assert

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

valid :: Array String
valid =
  [ "MIT"
  , "BSD-3-Clause"
  , "CC-BY-1.0"
  , "APACHE-2.0"

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
