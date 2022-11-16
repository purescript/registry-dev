module Test.Fixture.Manifest where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map as Map
import Data.Monoid (power)
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Registry.License (License)
import Registry.License as License
import Registry.Location (Location(..))
import Registry.Manifest (Manifest(..))
import Registry.Owner (Owner(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Version (Version)
import Registry.Version as Version

class Fixture a where
  fixture :: a

instance Fixture String where
  fixture = "fixture-string"

instance Fixture NonEmptyString where
  fixture = unsafeFromJust $ NonEmptyString.fromString "fixture-string"

instance Fixture PackageName where
  fixture = unsafeFromRight $ PackageName.parse "fixture-package-name"

instance Fixture Version where
  fixture = unsafeFromRight $ Version.parse "1.0.0"

instance Fixture License where
  fixture = unsafeFromRight $ License.parse "MIT"

instance Fixture Owner where
  fixture = Owner
    { email: "user@example.com"
    , keytype: "ssh-ed25519"
    , public: "AAAAC3NzaC1lZDI1NTE5AAAAIF6B6R9yp8yFM3wYcOhGO0EyZAefKWkvsTET+KeaegY/"
    }

instance Fixture Location where
  fixture = GitHub { owner: fixture, repo: fixture, subdir: Nothing }

instance Fixture (Map PackageName Range) where
  fixture = Map.empty

instance Fixture Manifest where
  fixture = Manifest
    { name: fixture
    , owners: fixture
    , version: fixture
    , license: fixture
    , location: fixture
    , description: fixture
    , files: fixture
    , dependencies: fixture
    }

instance (Fixture a) => Fixture (Maybe a) where
  fixture = Just fixture

instance (Fixture a) => Fixture (Array a) where
  fixture = [ fixture ]

instance (Fixture a) => Fixture (NonEmptyArray a) where
  fixture = NonEmptyArray.singleton fixture

setName :: String -> Manifest -> Manifest
setName name (Manifest manifest) =
  Manifest (manifest { name = unsafeFromRight $ PackageName.parse name })

setVersion :: String -> Manifest -> Manifest
setVersion version (Manifest manifest) =
  Manifest (manifest { version = unsafeFromRight $ Version.parse version })

setDescription :: String -> Manifest -> Manifest
setDescription description (Manifest manifest) =
  Manifest (manifest { description = Just description })

-- | Usage: setDependencies [ Tuple "package-name-one" "2.0.0", Tuple "package-name-two" "3.0.0" ]
setDependencies :: Array (Tuple String String) -> Manifest -> Manifest
setDependencies dependencies (Manifest manifest) =
  Manifest (manifest { dependencies = dependencies' })
  where
  dependencies' = Map.fromFoldable (map go dependencies)

  go (Tuple package version) =
    Tuple (unsafeFromRight (PackageName.parse package)) (mkRangeIncluding (unsafeFromRight (Version.parse version)))

  mkRangeIncluding :: Version -> Range
  mkRangeIncluding version = do
    let input = Array.fold [ ">=", Version.print version, " <", Version.print (Version.bumpPatch version) ]
    unsafeFromRight $ Range.parse input

-- | Creates a perfect binary tree of Manifests
-- | with all transitive dependencies contained in the resulting array.
-- | The entry at index 0 is the root.
-- | Int argument represents the desired depth of the binary tree & should be nonzero.
manifestTree :: Int -> Array Manifest
manifestTree depth =
  Array.mapWithIndex go init
  where
  init :: Array Manifest
  init = Array.replicate count fixture

  count = unwrap (power (Multiplicative 2) depth) - 1

  left ix = 2 * ix + 1

  right ix = 2 * ix + 2

  name :: Int -> String
  name ix = String.joinWith "-" [ fixture, show ix ]

  setName' = setName <<< name

  go ix manifest = do
    let
      deps =
        [ Tuple (name (left ix)) (Version.print fixture)
        , Tuple (name (right ix)) (Version.print fixture)
        ]

      updateDeps =
        if right ix < count then setDependencies deps else identity

    manifest
      # setName' ix
      # updateDeps
