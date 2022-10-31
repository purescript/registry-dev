module Test.Utils where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Partial.Unsafe as Partial
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Unsafe.Coerce (unsafeCoerce)

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

-- | Unsafely parse a sri-prefixed sha256 hash from a string
unsafeSha256 :: String -> Sha256.Sha256
unsafeSha256 = fromRight "Failed to parse Sha256" <<< Sha256.parse

-- | Unsafely parse a version from a string
unsafeVersion :: String -> Version.Version
unsafeVersion = fromRight "Failed to parse Version" <<< Version.parse

-- | Unsafely parse a range from a string
unsafeRange :: String -> Range.Range
unsafeRange = fromRight "Failed to parse Range" <<< Range.parse
