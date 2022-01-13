module Foreign.Jsonic (parseJsonic) where

import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)

foreign import parseJsonicImpl :: forall r. Fn3 (String -> r) (Json -> r) String r

parseJsonic :: String -> Either String Json
parseJsonic = runFn3 parseJsonicImpl Left Right
