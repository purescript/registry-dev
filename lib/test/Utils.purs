module Test.Utils where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Partial.Unsafe (unsafeCrashWith)
import Partial.Unsafe as Partial
import Registry.License as License
import Registry.Location (Location(..))
import Registry.Manifest (Manifest(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version (Version)
import Registry.Version as Version
import Unsafe.Coerce (unsafeCoerce)

-- | Unsafely unpack the `Just` of an `Maybe`, given a message to crash with
-- | if the value is a `Nothing`.
fromJust :: forall a. String -> Maybe a -> a
fromJust msg = case _ of
  Nothing -> unsafeCrashWith msg
  Just a -> a

-- | Unsafely unpack the `Right` of an `Either`, given a message to crash with
-- | if the value is a `Left`.
fromRight :: forall a b. String -> Either a b -> b
fromRight msg = Either.fromRight' (\_ -> Partial.unsafeCrashWith msg)

-- | Unsafely stringify a value by coercing it to `Json` and stringifying it.
unsafeStringify :: forall a. a -> String
unsafeStringify a = Argonaut.stringify (unsafeCoerce a :: Argonaut.Json)

-- | Partition an array of `Either` values into failure and success  values
partitionEithers :: forall e a. Array (Either e a) -> { fail :: Array e, success :: Array a }
partitionEithers = Array.foldMap case _ of
  Left err -> { fail: [ err ], success: [] }
  Right res -> { fail: [], success: [ res ] }

-- | Unsafely parse a license from a string
unsafeNonEmptyArray :: forall a. Array a -> NonEmptyArray.NonEmptyArray a
unsafeNonEmptyArray = fromJust "Failed to produce NonEmptyArray" <<< NonEmptyArray.fromArray

-- | Unsafely parse a sri-prefixed sha256 hash from a string
unsafeSha256 :: String -> Sha256.Sha256
unsafeSha256 = fromRight "Failed to parse Sha256" <<< Sha256.parse

-- | Unsafely parse a package name from a string
unsafePackageName :: String -> PackageName.PackageName
unsafePackageName = fromRight "Failed to parse PackageName" <<< PackageName.parse

-- | Unsafely parse a version from a string
unsafeVersion :: String -> Version.Version
unsafeVersion = fromRight "Failed to parse Version" <<< Version.parse

-- | Unsafely parse a range from a string
unsafeRange :: String -> Range.Range
unsafeRange = fromRight "Failed to parse Range" <<< Range.parse

-- | Unsafely parse a license from a string
unsafeLicense :: String -> License.License
unsafeLicense = fromRight "Failed to parse License" <<< License.parse

-- | Unsafely create a manifest from a name, version, and array of dependencies
-- | where keys are package names and values are ranges.
unsafeManifest :: String -> String -> Array (Tuple String String) -> Manifest
unsafeManifest name version dependencies = Manifest
  { name: unsafePackageName name
  , version: unsafeVersion version
  , dependencies: Map.fromFoldable $ map (bimap unsafePackageName unsafeRange) dependencies
  , license: unsafeLicense "MIT"
  , location: Git
      { url: "https://github.com/purescript/purescript-" <> name <> ".git"
      , subdir: Nothing
      }
  , description: Nothing
  , owners: Nothing
  , files: Nothing
  }

-- | Format a package version as a string in the form 'name@X.Y.Z'
formatPackageVersion :: PackageName -> Version -> String
formatPackageVersion name version = PackgaeName.print name <> "@" <> Version.print version
