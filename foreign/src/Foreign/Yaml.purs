module Registry.Foreign.Yaml
  ( yamlParser
  ) where

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import JSON (JSON)

-- | Parse a JSON string, constructing the `Toml` value described by the string.
-- | To convert a string into a `Toml` string, see `fromString`.
yamlParser :: String -> Either String JSON
yamlParser j = runFn3 yamlDocParserImpl Left Right j

foreign import yamlDocParserImpl :: forall a. Fn3 (String -> a) (JSON -> a) String a
