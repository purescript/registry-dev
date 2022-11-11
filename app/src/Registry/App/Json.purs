module Registry.App.Json
  ( module Core
  , module Parser
  , module Codec.Argonaut
  , module Codec.Argonaut.Record
  , decodeJson
  , printJson
  , stringifyJson
  , parseJson
  , writeJsonFile
  , readJsonFile
  , atKey
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringify, stringifyWithIndent) as Core
import Data.Argonaut.Parser (jsonParser) as Parser
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..), decode, encode, printJsonDecodeError) as Codec.Argonaut
import Data.Codec.Argonaut.Record (object) as Codec.Argonaut.Record
import Data.Either (Either(..))
import Data.Maybe as Maybe
import Effect.Aff (Aff, try)
import Effect.Aff as Aff
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)

-- | Decode JSON into a type with the given codec
decodeJson :: forall a. Codec.Argonaut.JsonCodec a -> Core.Json -> Either String a
decodeJson codec = lmap Codec.Argonaut.printJsonDecodeError <<< Codec.Argonaut.decode codec

-- | Print a type as a formatted JSON string
printJson :: forall a. Codec.Argonaut.JsonCodec a -> a -> String
printJson codec = Core.stringifyWithIndent 2 <<< Codec.Argonaut.encode codec

-- | Print a type as a JSON string without formatting
stringifyJson :: forall a. Codec.Argonaut.JsonCodec a -> a -> String
stringifyJson codec = Core.stringify <<< Codec.Argonaut.encode codec

-- | Parse a type from a string of JSON data.
parseJson :: forall a. Codec.Argonaut.JsonCodec a -> String -> Either String a
parseJson codec = decodeJson codec <=< Parser.jsonParser

-- | Encode data as formatted JSON and write it to the provided filepath
writeJsonFile :: forall a. Codec.Argonaut.JsonCodec a -> FilePath -> a -> Aff Unit
writeJsonFile codec path = FS.writeTextFile UTF8 path <<< (_ <> "\n") <<< printJson codec

-- | Decode data from a JSON file at the provided filepath
readJsonFile :: forall a. Codec.Argonaut.JsonCodec a -> FilePath -> Aff (Either String a)
readJsonFile codec path = do
  result <- try $ FS.readTextFile UTF8 path
  pure (lmap Aff.message result >>= parseJson codec)

-- | Decode the value of a field from an Object Json using the given codec.
-- | Useful for ad-hoc decoders.
atKey :: forall a. String -> Codec.Argonaut.JsonCodec a -> Object Core.Json -> Either String a
atKey key codec object =
  Maybe.maybe
    (Left $ Codec.Argonaut.printJsonDecodeError $ Codec.Argonaut.AtKey key Codec.Argonaut.MissingValue)
    (lmap (Codec.Argonaut.printJsonDecodeError <<< Codec.Argonaut.AtKey key) <<< Codec.Argonaut.decode codec)
    (Object.lookup key object)
