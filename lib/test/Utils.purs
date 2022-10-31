module Test.Utils where

import Data.Argonaut.Core as Argonaut
import Data.Either (Either)
import Data.Either as Either
import Partial.Unsafe as Partial
import Unsafe.Coerce (unsafeCoerce)

fromRight :: forall a b. String -> Either a b -> b
fromRight msg = Either.fromRight' (\_ -> Partial.unsafeCrashWith msg)

unsafeStringify :: forall a. a -> String
unsafeStringify a = Argonaut.stringify (unsafeCoerce a :: Argonaut.Json)
