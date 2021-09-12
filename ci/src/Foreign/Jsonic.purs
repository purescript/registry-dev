module Foreign.Jsonic (parse) where

import Registry.Prelude

import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn3, runFn3)

parse :: String -> Either String Json
parse = runFn3 parseJsonicImpl Left Right

foreign import parseJsonicImpl :: forall r. Fn3 (String -> r) (Json -> r) String r
