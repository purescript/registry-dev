module Registry.Test.Fixtures where

import Prelude

import Data.Either as Either
import Data.Maybe (Maybe(..))
import Partial.Unsafe as Partial
import Registry.Location (Location(..))
import Registry.Sha256 (Sha256)
import Registry.Sha256 as Sha256

-- | A Location for use within tests.
defaultLocation :: Location
defaultLocation = GitHub { owner: "purescript", repo: "registry-dev", subdir: Nothing }

-- | A Sha256 for use within tests.
defaultHash :: Sha256
defaultHash = Either.fromRight' (\_ -> Partial.unsafeCrashWith "Failed to parse Sha256") $ Sha256.parse "sha256-fN9RUAzN21ZY4Y0UwqUSxwUPVz1g7/pcqoDvbJZoT04="
