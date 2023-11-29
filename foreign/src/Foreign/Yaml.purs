module Foreign.Yaml
  ( yamlParser
  ) where

import Registry.App.Prelude

import Data.Argonaut.Core as Core
import Data.Function.Uncurried (Fn3, runFn3)

-- | Parse a JSON string, constructing the `Toml` value described by the string.
-- | To convert a string into a `Toml` string, see `fromString`.
yamlParser :: String -> Either String Core.Json
yamlParser j = runFn3 yamlDocParserImpl Left Right j

foreign import yamlDocParserImpl :: forall a. Fn3 (String -> a) (Core.Json -> a) String a

