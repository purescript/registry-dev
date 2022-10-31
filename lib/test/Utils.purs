module Test.Utils where

import Data.Argonaut.Core as Argonaut
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Partial.Unsafe as Partial
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
