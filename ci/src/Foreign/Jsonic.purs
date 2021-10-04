module Foreign.Jsonic (parseJson) where

import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)

-- | A drop-in replacement for `Data.Argonaut.parseJson` that uses a lenient
-- | JSON parser.
parseJson :: String -> Either Json.JsonDecodeError Json
parseJson input = do
  let toJsonError e = Json.Named "Malformed JSON" (Json.UnexpectedValue (Json.fromString e))
  lmap toJsonError (parseJsonic input)

foreign import parseJsonicImpl :: forall r. Fn3 (String -> r) (Json -> r) String r

parseJsonic :: String -> Either String Json
parseJsonic = runFn3 parseJsonicImpl Left Right
