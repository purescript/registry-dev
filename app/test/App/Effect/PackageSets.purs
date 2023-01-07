module Test.Registry.App.Effect.PackageSets (spec) where

import Registry.App.Prelude

import Data.Map as Map
import Registry.App.Effect.PackageSets (Change(..))
import Registry.App.Effect.PackageSets as PackageSets
import Registry.Version as Version
import Test.Assert as Assert
import Test.Spec as Spec
import Test.Utils as Utils

spec :: Spec.Spec Unit
spec = do
  Spec.it "Reports package set changelog correctly" do
    let
      operations = Map.fromFoldable
        [ map (const Remove) assert
        , map (Version.bumpPatch >>> Update) effect
        , Tuple (Utils.unsafePackageName "math") (Update (Utils.unsafeVersion "1.0.0"))
        ]

    PackageSets.commitMessage packageSet operations (Utils.unsafeVersion "2.0.0") `Assert.shouldEqual` packageSetCommitMessage

  Spec.it "Reports package set changelog correctly (no updates)" do
    let
      operations = Map.fromFoldable
        [ map (const Remove) assert
        , Tuple (Utils.unsafePackageName "math") (Update (Utils.unsafeVersion "1.0.0"))
        ]

    PackageSets.commitMessage packageSet operations (Utils.unsafeVersion "2.0.0") `Assert.shouldEqual` packageSetCommitMessageNoUpdates

packageSet :: PackageSet
packageSet = PackageSet
  { compiler: Utils.unsafeVersion "0.15.2"
  , published: Utils.unsafeDate "2022-07-25"
  , version: Utils.unsafeVersion "4.2.0"
  , packages: Map.fromFoldable do
      [ assert
      , console
      , effect
      , prelude
      ]
  }

assert :: Tuple PackageName Version
assert = Tuple (Utils.unsafePackageName "assert") (Utils.unsafeVersion "6.0.0")

console :: Tuple PackageName Version
console = Tuple (Utils.unsafePackageName "console") (Utils.unsafeVersion "6.0.0")

effect :: Tuple PackageName Version
effect = Tuple (Utils.unsafePackageName "effect") (Utils.unsafeVersion "4.0.0")

prelude :: Tuple PackageName Version
prelude = Tuple (Utils.unsafePackageName "prelude") (Utils.unsafeVersion "6.0.0")

packageSetCommitMessage :: String
packageSetCommitMessage =
  """Release 2.0.0 package set.

New packages:
  - math@1.0.0

Updated packages:
  - effect@4.0.0 -> 4.0.1

Removed packages:
  - assert@6.0.0
"""

packageSetCommitMessageNoUpdates :: String
packageSetCommitMessageNoUpdates =
  """Release 2.0.0 package set.

New packages:
  - math@1.0.0

Removed packages:
  - assert@6.0.0
"""
