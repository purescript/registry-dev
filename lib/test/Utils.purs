module Test.Utils where

import Data.Either (Either)
import Data.Either as Either
import Partial.Unsafe as Partial

fromRight :: forall a b. String -> Either a b -> b
fromRight msg = Either.fromRight' (\_ -> Partial.unsafeCrashWith msg)
