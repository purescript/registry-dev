module Registry.Test.Utils where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.DateTime as Date
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Either as Either
import Data.Formatter.DateTime as DateTime.Formatters
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Partial.Unsafe (unsafeCrashWith)
import Partial.Unsafe as Partial
import Registry.Internal.Format as Internal.Format
import Registry.License as License
import Registry.Location (Location(..))
import Registry.Manifest (Manifest(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.SSH as SSH
import Registry.Sha256 (Sha256)
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
unsafeNonEmptyArray arr = fromJust ("Failed to produce NonEmptyArray: " <> unsafeStringify arr) (NonEmptyArray.fromArray arr)

-- | Unsafely parse a sri-prefixed sha256 hash from a string
unsafeSha256 :: String -> Sha256.Sha256
unsafeSha256 str = fromRight ("Failed to parse Sha256: " <> str) (Sha256.parse str)

-- | Unsafely parse a package name from a string
unsafePackageName :: String -> PackageName.PackageName
unsafePackageName str = fromRight ("Failed to parse PackageName: " <> str) (PackageName.parse str)

-- | Unsafely parse a version from a string
unsafeVersion :: String -> Version.Version
unsafeVersion str = fromRight ("Failed to parse Version: " <> str) (Version.parse str)

-- | Unsafely parse a range from a string
unsafeRange :: String -> Range.Range
unsafeRange str = fromRight ("Failed to parse Range: " <> str) (Range.parse str)

-- | Unsafely parse a license from a string
unsafeLicense :: String -> License.License
unsafeLicense str = fromRight ("Failed to parse License: " <> str) (License.parse str)

-- | Unsafely parse a DateTime from an ISO8601 datetime string
unsafeDateTime :: String -> DateTime.DateTime
unsafeDateTime str = fromRight ("Failed to parse DateTime: " <> str) (DateTime.Formatters.unformat Internal.Format.iso8601DateTime str)

-- | Unsafely parse a Date from an ISO8601 string
unsafeDate :: String -> Date.Date
unsafeDate str = fromRight ("Failed to parse Date: " <> str) (map Date.date $ DateTime.Formatters.unformat Internal.Format.iso8601Date str)

-- | Unsafely parse a public SSH key from a string
unsafeSSHPublicKey :: String -> SSH.PublicKey
unsafeSSHPublicKey str = fromRight ("Failed to parse SSH key: " <> str) (SSH.parsePublicKey str)

-- | Unsafely parse a private SSH key from a string
unsafeSSHPrivateKey :: String -> SSH.PrivateKey
unsafeSSHPrivateKey str = fromRight ("Failed to parse SSH key: " <> str) (SSH.parsePrivateKey str)

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
formatPackageVersion name version = PackageName.print name <> "@" <> Version.print version

-- | A Location for use within tests.
defaultLocation :: Location
defaultLocation = GitHub { owner: "purescript", repo: "registry-dev", subdir: Nothing }

-- | A Sha256 for use within tests.
defaultHash :: Sha256
defaultHash = fromRight "Failed to parse Sha256" $ Sha256.parse "sha256-fN9RUAzN21ZY4Y0UwqUSxwUPVz1g7/pcqoDvbJZoT04="
