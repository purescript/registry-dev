module Registry.Test.Utils where

import Prelude

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
import JSON (JSON)
import JSON as JSON
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
import Registry.Sha256 as Sha256
import Registry.Version (Version)
import Registry.Version as Version
import Unsafe.Coerce (unsafeCoerce)

foreign import archImpl :: String
foreign import platformImpl :: String

-- | CPU architecture as reported by Node.js process.arch
data Arch = ARM64 | X64 | UnknownArch String

derive instance Eq Arch

-- | Operating system platform as reported by Node.js process.platform
data Platform = Darwin | Linux | UnknownPlatform String

derive instance Eq Platform

-- | Get the current CPU architecture
arch :: Arch
arch = case archImpl of
  "arm64" -> ARM64
  "x64" -> X64
  other -> UnknownArch other

-- | Get the current OS platform
platform :: Platform
platform = case platformImpl of
  "darwin" -> Darwin
  "linux" -> Linux
  other -> UnknownPlatform other

-- | Returns true if running on Apple Silicon (aarch64-darwin)
isAarch64Darwin :: Boolean
isAarch64Darwin = arch == ARM64 && platform == Darwin

-- | The minimum compiler version with native aarch64-darwin binaries.
-- | Earlier versions only have x86_64-linux and x86_64-darwin builds.
minAarch64DarwinCompiler :: Version
minAarch64DarwinCompiler = unsafeVersion "0.15.9"

-- | Filter compiler versions to only those available on the current platform.
-- | On aarch64-darwin, compilers before 0.15.9 don't have native binaries,
-- | so tests for those versions should be skipped.
filterAvailableCompilers :: Array Version -> Array Version
filterAvailableCompilers versions
  | isAarch64Darwin = Array.filter (\v -> v >= minAarch64DarwinCompiler) versions
  | otherwise = versions

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
unsafeStringify a = JSON.print (unsafeCoerce a :: JSON)

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
unsafeSSHPrivateKey str = fromRight ("Failed to parse SSH key: " <> str) (SSH.parsePrivateKey { key: str, passphrase: Nothing })

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
  , includeFiles: Nothing
  , excludeFiles: Nothing
  }

-- | Format a package version as a string in the form 'name@X.Y.Z'
formatPackageVersion :: PackageName -> Version -> String
formatPackageVersion name version = PackageName.print name <> "@" <> Version.print version
