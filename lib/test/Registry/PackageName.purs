module Test.Registry.PackageName (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Registry.PackageName as PackageName
import Test.Assert as Assert
import Test.Spec as Spec
import Test.Utils as Utils

spec :: Spec.Spec Unit
spec = do
  Spec.it "Parses well-formed package names" do
    let
      parse name = case PackageName.parse name of
        Left error -> Left (name /\ error)
        Right value -> Right (PackageName.print value)

      formatError (Tuple name error) = name <> ": " <> error

      { fail } = Utils.partitionEithers $ map parse goodPackageNames

    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some well-formed package names were not parsed correctly:"
        , String.joinWith "\n  - " $ map formatError fail
        ]

  Spec.it "Fails to parse malformed package names" do
    let
      parse (Tuple name expectedError) = case PackageName.parse name of
        Left error | error == expectedError -> Right name
        Left error -> Left { name, expectedError, actualError: Just error }
        Right _ -> Left { name, expectedError, actualError: Nothing }

      formatError { name, expectedError, actualError } = Array.foldMap (append "\n  ") $ case actualError of
        Nothing -> [ name, "...should have failed with '" <> expectedError <> "'", "...but succeeded instead." ]
        Just error -> [ name, "...should have failed with '" <> expectedError <> "'", "...but failed with '" <> error <> "' instead." ]

      { fail } = Utils.partitionEithers $ map parse badPackageNames

    unless (Array.null fail) do
      Assert.fail $ String.joinWith "\n"
        [ "Some malformed package names were not parsed correctly:"
        , String.joinWith "\n  - " $ map formatError fail
        ]

goodPackageNames :: Array String
goodPackageNames =
  -- standard package names
  [ "a"
  , "ab"
  , "abc"
  , "prelude"
  , "codec-argonaut"

  -- blessed purescript-prefixed packages
  , "purescript-compiler-backend-utilities"
  ]

badPackageNames :: Array (Tuple String String)
badPackageNames = do
  let startErr = "Package name should start with a lower case char or a digit"
  let midErr = "Package name can contain lower case chars, digits and non-consecutive dashes"
  let prefixErr = "Package names should not begin with 'purescript-'"
  let endErr = "Package name should end with a lower case char or digit"
  let manyDashesErr = "Package names cannot contain consecutive dashes"
  let expectedEOF = "Expected EOF"

  [ "-a" /\ startErr
  , "double--dash" /\ manyDashesErr
  , "BIGLETTERS" /\ startErr
  , "some space" /\ midErr
  , "a-" /\ endErr
  , "" /\ startErr
  , "🍝" /\ startErr
  , "abc-🍝-abc" /\ expectedEOF
  , "purescript-aff" /\ prefixErr
  ]
