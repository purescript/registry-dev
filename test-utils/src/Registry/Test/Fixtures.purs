module Registry.Test.Fixtures where

import Prelude

import Data.Either as Either
import Data.Maybe (Maybe(..))
import Partial.Unsafe as Partial
import Registry.Location (Location(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Sha256 (Sha256)
import Registry.Sha256 as Sha256
import Registry.Version (Version)
import Registry.Version as Version

type PackageFixture = { name :: PackageName, version :: Version }

unsafePackageName :: String -> PackageName
unsafePackageName str = Either.fromRight' (\_ -> Partial.unsafeCrashWith $ "Failed to parse PackageName: " <> str) (PackageName.parse str)

unsafeVersion :: String -> Version
unsafeVersion str = Either.fromRight' (\_ -> Partial.unsafeCrashWith $ "Failed to parse Version: " <> str) (Version.parse str)

-- Common test fixture packages (match app/fixtures/github-packages)

-- | effect@4.0.0 fixture package
effect :: PackageFixture
effect = { name: unsafePackageName "effect", version: unsafeVersion "4.0.0" }

-- | console@6.1.0 fixture package
console :: PackageFixture
console = { name: unsafePackageName "console", version: unsafeVersion "6.1.0" }

-- | prelude@6.0.1 fixture package
prelude :: PackageFixture
prelude = { name: unsafePackageName "prelude", version: unsafeVersion "6.0.1" }

-- | type-equality@4.0.1 fixture package
typeEquality :: PackageFixture
typeEquality = { name: unsafePackageName "type-equality", version: unsafeVersion "4.0.1" }

-- | A Location for use within tests.
defaultLocation :: Location
defaultLocation = GitHub { owner: "purescript", repo: "registry-dev", subdir: Nothing }

-- | A Sha256 for use within tests.
defaultHash :: Sha256
defaultHash = Either.fromRight' (\_ -> Partial.unsafeCrashWith "Failed to parse Sha256") $ Sha256.parse "sha256-fN9RUAzN21ZY4Y0UwqUSxwUPVz1g7/pcqoDvbJZoT04="
