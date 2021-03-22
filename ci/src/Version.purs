module Registry.Version
  ( Version
  , parse
  , print
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)

newtype Version = Version String

derive newtype instance eqVersion :: Eq Version

instance decodeJsonVersion :: DecodeJson Version where
  decodeJson json = do
    version <- decodeJson json
    parse version # lmap (TypeMismatch <<< append "Expected Version: ")

instance encodeJsonVersion :: EncodeJson Version where
  encodeJson = encodeJson <<< print

print :: Version -> String
print (Version package) = package

-- FIXME: This is a stub implementation for proper version parsing, with good
-- error messages.
parse :: String -> Either String Version
parse = pure <<< Version
