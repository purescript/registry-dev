module Test.Registry.PackageSet
  ( spec
  ) where

import Registry.Prelude

import Data.DateTime (DateTime)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.RFC3339String (RFC3339String(..))
import Registry.Hash (unsafeSha256)
import Registry.Json as Json
import Registry.Legacy.PackageSet (ConvertedLegacyPackageSet)
import Registry.Legacy.PackageSet as Legacy.PackageSet
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location(..), Manifest, Metadata, PackageSet(..))
import Registry.Schema as Schema
import Registry.Version (Version)
import Registry.Version as Version
import Test.Fixture.Manifest (fixture, setDependencies, setName, setVersion)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec.Spec Unit
spec = do
  Spec.it "Decodes package set" do
    Json.parseJson packageSetJson `Assert.shouldContain` packageSet

  Spec.it "Encodes package set" do
    Json.printJson packageSet `Assert.shouldEqual` packageSetJson

  Spec.it "Produces correct legacy package set name" do
    convertedPackageSet.name `Assert.shouldEqual` "psc-0.15.2-20220725"

  Spec.it "Decodes legacy package set" do
    Json.parseJson legacyPackageSetJson `Assert.shouldContain` convertedPackageSet.packageSet

  Spec.it "Encodes legacy package set as JSON" do
    Json.printJson convertedPackageSet.packageSet `Assert.shouldEqual` legacyPackageSetJson

  Spec.it "Encodes legacy package set as Dhall" do
    Legacy.PackageSet.printDhall convertedPackageSet.packageSet `Assert.shouldEqual` legacyPackageSetDhall

packageSet :: PackageSet
packageSet = PackageSet
  { compiler: unsafeVersion "0.15.2"
  , published: unsafeDate "2022-07-25"
  , version: unsafeVersion "4.2.0"
  , packages: Map.fromFoldable $ map (bimap unsafeName unsafeVersion) do
      [ assert
      , console
      , effect
      , prelude
      ]
  }

packageSetJson :: String
packageSetJson =
  """{
  "version": "4.2.0",
  "compiler": "0.15.2",
  "published": "2022-07-25",
  "packages": {
    "assert": "6.0.0",
    "console": "6.0.0",
    "effect": "4.0.0",
    "prelude": "6.0.0"
  }
}"""

convertedPackageSet :: ConvertedLegacyPackageSet
convertedPackageSet =
  case Legacy.PackageSet.fromPackageSet index metadata packageSet of
    Left err -> unsafeCrashWith err
    Right value -> value
  where
  index = Map.fromFoldable
    [ unsafeIndexEntry assert [ console, effect, prelude ]
    , unsafeIndexEntry console [ effect, prelude ]
    , unsafeIndexEntry effect [ prelude ]
    , unsafeIndexEntry prelude []
    ]

  metadata = Map.fromFoldable
    [ unsafeMetadataEntry assert
    , unsafeMetadataEntry console
    , unsafeMetadataEntry effect
    , unsafeMetadataEntry prelude
    ]

legacyPackageSetJson :: String
legacyPackageSetJson =
  """{
  "assert": {
    "dependencies": [
      "console",
      "effect",
      "prelude"
    ],
    "repo": "https://github.com/purescript/purescript-assert.git",
    "version": "v6.0.0"
  },
  "console": {
    "dependencies": [
      "effect",
      "prelude"
    ],
    "repo": "https://github.com/purescript/purescript-console.git",
    "version": "v6.0.0"
  },
  "effect": {
    "dependencies": [
      "prelude"
    ],
    "repo": "https://github.com/purescript/purescript-effect.git",
    "version": "v4.0.0"
  },
  "metadata": {
    "dependencies": [],
    "repo": "https://github.com/purescript/purescript-metadata.git",
    "version": "v0.15.2"
  },
  "prelude": {
    "dependencies": [],
    "repo": "https://github.com/purescript/purescript-prelude.git",
    "version": "v6.0.0"
  }
}"""

legacyPackageSetDhall :: String
legacyPackageSetDhall =
  """{
  `assert` =
    { dependencies = [ "console", "effect", "prelude" ]
    , repo = "https://github.com/purescript/purescript-assert.git"
    , version = "v6.0.0"
    }
  , console =
    { dependencies = [ "effect", "prelude" ]
    , repo = "https://github.com/purescript/purescript-console.git"
    , version = "v6.0.0"
    }
  , effect =
    { dependencies = [ "prelude" ]
    , repo = "https://github.com/purescript/purescript-effect.git"
    , version = "v4.0.0"
    }
  , metadata =
    { dependencies = [] : List Text
    , repo = "https://github.com/purescript/purescript-metadata.git"
    , version = "v0.15.2"
    }
  , prelude =
    { dependencies = [] : List Text
    , repo = "https://github.com/purescript/purescript-prelude.git"
    , version = "v6.0.0"
    }
}"""

assert :: Tuple String String
assert = Tuple "assert" "6.0.0"

console :: Tuple String String
console = Tuple "console" "6.0.0"

effect :: Tuple String String
effect = Tuple "effect" "4.0.0"

prelude :: Tuple String String
prelude = Tuple "prelude" "6.0.0"

unsafeName :: String -> PackageName
unsafeName = unsafeFromRight <<< PackageName.parse

unsafeVersion :: String -> Version
unsafeVersion = unsafeFromRight <<< Version.parseVersion Version.Lenient

unsafeDate :: String -> DateTime
unsafeDate = unsafeFromRight <<< Formatter.DateTime.unformat Schema.dateFormatter

unsafeIndexEntry :: Tuple String String -> Array (Tuple String String) -> Tuple PackageName (Map Version Manifest)
unsafeIndexEntry (Tuple name version) deps = do
  let
    manifest =
      fixture
        # setName name
        # setVersion version
        # setDependencies deps

  unsafeName name /\ Map.singleton (unsafeVersion version) manifest

unsafeMetadataEntry :: Tuple String String -> Tuple PackageName Metadata
unsafeMetadataEntry (Tuple name version) = do
  let
    published =
      { ref: "v" <> version
      , hash: unsafeSha256 "sha256-gb24ZRec6mgR8TFBVR2eIh5vsMdhuL+zK9VKjWP74Cw="
      , bytes: 0.0
      , publishedTime: RFC3339String ""
      }

    metadata =
      { location: GitHub { owner: "purescript", repo: "purescript-" <> name, subdir: Nothing }
      , owners: Nothing
      , published: Map.singleton (unsafeVersion version) published
      , unpublished: Map.empty
      }

  Tuple (unsafeName name) metadata
