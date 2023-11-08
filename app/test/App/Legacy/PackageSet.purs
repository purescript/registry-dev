module Test.Registry.App.Legacy.PackageSet (spec) where

import Registry.App.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime (DateTime(..))
import Data.Either as Either
import Data.Map as Map
import Data.Set as Set
import Registry.App.Legacy.LenientVersion (LenientVersion)
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.App.Legacy.PackageSet (ConvertedLegacyPackageSet, LatestCompatibleSets, latestCompatibleSetsCodec, parsePscTag, printPscTag)
import Registry.App.Legacy.PackageSet as Legacy.PackageSet
import Registry.App.Legacy.Types (legacyPackageSetCodec)
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Sha256 as Sha256
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Registry.Version as Version
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "Parses legacy package set tags" do
    let valid = "psc-0.12.18-20220102"
    let invalid = "psc-0.14.3.1-2022-01-02"
    let roundtrip = map printPscTag <<< parsePscTag
    roundtrip valid `Assert.shouldContain` valid
    roundtrip invalid `Assert.shouldSatisfy` Either.isLeft

  Spec.it "Parses legacy 'latest compatible set' files" do
    let
      parsed :: Either _ LatestCompatibleSets
      parsed = parseJson latestCompatibleSetsCodec latestCompatibleSets

    map (printJson latestCompatibleSetsCodec) parsed `Assert.shouldContain` latestCompatibleSets

  Spec.it "Produces correct legacy package set name" do
    printPscTag convertedPackageSet.tag `Assert.shouldEqual` "psc-0.15.2-20220725"

  Spec.it "Decodes legacy package set" do
    parseJson legacyPackageSetCodec legacyPackageSetJson `Assert.shouldContain` convertedPackageSet.packageSet

  Spec.it "Encodes legacy package set as JSON" do
    printJson legacyPackageSetCodec convertedPackageSet.packageSet `Assert.shouldEqual` legacyPackageSetJson

  Spec.it "Encodes legacy package set as Dhall" do
    Legacy.PackageSet.printDhall convertedPackageSet.packageSet `Assert.shouldEqual` legacyPackageSetDhall

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
  { compiler: Utils.unsafeVersion "0.15.2"
  , published: Utils.unsafeDate "2022-07-25"
  , version: Utils.unsafeVersion "4.2.0"
  , packages: map LenientVersion.version $ Map.fromFoldable do
      [ assert
      , console
      , effect
      , prelude
      ]
  }

convertedPackageSet :: ConvertedLegacyPackageSet
convertedPackageSet =
  case Legacy.PackageSet.convertPackageSet index metadata packageSet of
    Left err -> unsafeCrashWith err
    Right value -> value
  where
  index = unsafeFromRight $ ManifestIndex.fromSet $ Set.fromFoldable
    [ mkManifest assert [ console, effect, prelude ]
    , mkManifest console [ effect, prelude ]
    , mkManifest effect [ prelude ]
    , mkManifest prelude []
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

assert :: Tuple PackageName LenientVersion
assert = Tuple (Utils.unsafePackageName "assert") (unsafeLenientVersion "v6.0.0")

console :: Tuple PackageName LenientVersion
console = Tuple (Utils.unsafePackageName "console") (unsafeLenientVersion "v6.0.0")

effect :: Tuple PackageName LenientVersion
effect = Tuple (Utils.unsafePackageName "effect") (unsafeLenientVersion "v4.0.0")

prelude :: Tuple PackageName LenientVersion
prelude = Tuple (Utils.unsafePackageName "prelude") (unsafeLenientVersion "v6.0.0")

unsafeLenientVersion :: String -> LenientVersion
unsafeLenientVersion = unsafeFromRight <<< LenientVersion.parse

mkManifest :: Tuple PackageName LenientVersion -> Array (Tuple PackageName LenientVersion) -> Manifest
mkManifest (Tuple name version) deps = do
  let toRange v = ">=" <> Version.print v <> " <" <> Version.print (Version.bumpHighest v)
  Utils.unsafeManifest
    (PackageName.print name)
    (LenientVersion.print version)
    (map (bimap PackageName.print (LenientVersion.version >>> toRange)) deps)

unsafeMetadataEntry :: Tuple PackageName LenientVersion -> Tuple PackageName Metadata
unsafeMetadataEntry (Tuple name version) = do
  let
    published =
      { ref: LenientVersion.raw version
      , hash: unsafeFromRight $ Sha256.parse "sha256-gb24ZRec6mgR8TFBVR2eIh5vsMdhuL+zK9VKjWP74Cw="
      , bytes: 0.0
      , compilers: Right (NonEmptyArray.singleton (Utils.unsafeVersion "0.15.2"))
      , publishedTime: DateTime (Utils.unsafeDate "2022-07-07") bottom
      }

    metadata = Metadata
      { location: GitHub { owner: "purescript", repo: "purescript-" <> PackageName.print name, subdir: Nothing }
      , owners: Nothing
      , published: Map.singleton (LenientVersion.version version) published
      , unpublished: Map.empty
      }

  Tuple name metadata
