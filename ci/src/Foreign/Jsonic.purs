module Foreign.Jsonic (parseJson) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)

-- | A drop-in replacement for `Data.Argonaut.parseJson` that uses a lenient
-- | JSON parser.
parseJson :: String -> Either CA.JsonDecodeError Json
parseJson input = do
  let toJsonError e = CA.Named "Malformed JSON" (CA.UnexpectedValue (Json.fromString e))
  lmap toJsonError (parseJsonic input)

foreign import parseJsonicImpl :: forall r. Fn3 (String -> r) (Json -> r) String r

parseJsonic :: String -> Either String Json
parseJsonic = runFn3 parseJsonicImpl Left Right
