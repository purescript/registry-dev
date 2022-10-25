module Test.Registry.PackageSet
  ( spec
  ) where

import Registry.Prelude

import Data.DateTime (DateTime)
import Data.Either as Either
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.RFC3339String (RFC3339String(..))
import Registry.Json as Json
import Registry.Legacy.PackageSet (ConvertedLegacyPackageSet, LatestCompatibleSets, parsePscTag, printPscTag)
import Registry.Legacy.PackageSet as Legacy.PackageSet
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.SRIHash as SRIHash
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

  Spec.it "Parses legacy package set tags" do
    let valid = "psc-0.12.18-20220102"
    let invalid = "psc-0.14.3.1-2022-01-02"
    let roundtrip = map printPscTag <<< parsePscTag
    roundtrip valid `Assert.shouldContain` valid
    roundtrip invalid `Assert.shouldSatisfy` Either.isLeft

  Spec.it "Parses legacy 'latest compatible set' files" do
    let
      parsed :: Either _ LatestCompatibleSets
      parsed = Json.parseJson latestCompatibleSets

    map Json.printJson parsed `Assert.shouldContain` latestCompatibleSets

  Spec.it "Produces correct legacy package set name" do
    printPscTag convertedPackageSet.tag `Assert.shouldEqual` "psc-0.15.2-20220725"

  Spec.it "Decodes legacy package set" do
    Json.parseJson legacyPackageSetJson `Assert.shouldContain` convertedPackageSet.packageSet

  Spec.it "Encodes legacy package set as JSON" do
    Json.printJson convertedPackageSet.packageSet `Assert.shouldEqual` legacyPackageSetJson

  Spec.it "Encodes legacy package set as Dhall" do
    Legacy.PackageSet.printDhall convertedPackageSet.packageSet `Assert.shouldEqual` legacyPackageSetDhall

  Spec.it "Reports package set changelog correctly" do
    let
      operations = Map.fromFoldable
        [ map (const Nothing) assert
        , map (Version.bumpPatch >>> Just) effect
        , Tuple (unsafeName "math") (Just (unsafeVersion "1.0.0"))
        ]

    PackageSet.commitMessage packageSet operations (unsafeVersion "2.0.0") `Assert.shouldEqual` packageSetCommitMessage

  Spec.it "Reports package set changelog correctly (no updates)" do
    let
      operations = Map.fromFoldable
        [ map (const Nothing) assert
        , Tuple (unsafeName "math") (Just (unsafeVersion "1.0.0"))
        ]

    PackageSet.commitMessage packageSet operations (unsafeVersion "2.0.0") `Assert.shouldEqual` packageSetCommitMessageNoUpdates

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

latestCompatibleSets :: String
latestCompatibleSets =
  """{
  "0.12.0": "psc-0.12.0-20181024",
  "0.12.1": "psc-0.12.1-20190107",
  "0.12.2": "psc-0.12.2-20190210",
  "0.12.3": "psc-0.12.3-20190409",
  "0.12.4": "psc-0.12.4-20190418",
  "0.12.5": "psc-0.12.5-20190525",
  "0.13.0": "psc-0.13.0-20190713",
  "0.13.2": "psc-0.13.2-20190815",
  "0.13.3": "psc-0.13.3-20191005",
  "0.13.4": "psc-0.13.4-20191125",
  "0.13.5": "psc-0.13.5-20200103",
  "0.13.6": "psc-0.13.6-20200507",
  "0.13.8": "psc-0.13.8-20210226",
  "0.14.0": "psc-0.14.0-20210409",
  "0.14.1": "psc-0.14.1-20210613",
  "0.14.2": "psc-0.14.2-20210713",
  "0.14.3": "psc-0.14.3-20210825",
  "0.14.4": "psc-0.14.4-20211109",
  "0.14.5": "psc-0.14.5-20220224",
  "0.14.6": "psc-0.14.6-20220228",
  "0.14.7": "psc-0.14.7-20220418",
  "0.15.0": "psc-0.15.0-20220527",
  "0.15.2": "psc-0.15.2-20220706",
  "0.15.3": "psc-0.15.3-20220712",
  "0.15.4": "psc-0.15.4-20220829"
}"""

packageSet :: PackageSet
packageSet = PackageSet
  { compiler: unsafeVersion "0.15.2"
  , published: unsafeDate "2022-07-25"
  , version: unsafeVersion "4.2.0"
  , packages: Map.fromFoldable do
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
  """{ `assert` =
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

assert :: Tuple PackageName Version
assert = Tuple (unsafeName "assert") (unsafeVersion "v6.0.0")

console :: Tuple PackageName Version
console = Tuple (unsafeName "console") (unsafeVersion "v6.0.0")

effect :: Tuple PackageName Version
effect = Tuple (unsafeName "effect") (unsafeVersion "v4.0.0")

prelude :: Tuple PackageName Version
prelude = Tuple (unsafeName "prelude") (unsafeVersion "v6.0.0")

unsafeName :: String -> PackageName
unsafeName = unsafeFromRight <<< PackageName.parse

unsafeVersion :: String -> Version
unsafeVersion = unsafeFromRight <<< Version.parseVersion Version.Lenient

unsafeDate :: String -> DateTime
unsafeDate = unsafeFromRight <<< Formatter.DateTime.unformat Schema.dateFormatter

unsafeIndexEntry :: Tuple PackageName Version -> Array (Tuple PackageName Version) -> Tuple PackageName (Map Version Manifest)
unsafeIndexEntry (Tuple name version) deps = do
  let
    manifest =
      fixture
        # setName (PackageName.print name)
        # setVersion (Version.rawVersion version)
        # setDependencies (map (bimap PackageName.print Version.rawVersion) deps)

  name /\ Map.singleton version manifest

unsafeMetadataEntry :: Tuple PackageName Version -> Tuple PackageName Metadata
unsafeMetadataEntry (Tuple name version) = do
  let
    published =
      { ref: Version.rawVersion version
      , hash: unsafeFromRight $ SRIHash.parse "sha256-gb24ZRec6mgR8TFBVR2eIh5vsMdhuL+zK9VKjWP74Cw="
      , bytes: 0.0
      , publishedTime: RFC3339String ""
      }

    metadata =
      { location: GitHub { owner: "purescript", repo: "purescript-" <> PackageName.print name, subdir: Nothing }
      , owners: Nothing
      , published: Map.singleton version published
      , unpublished: Map.empty
      }

  Tuple name metadata
