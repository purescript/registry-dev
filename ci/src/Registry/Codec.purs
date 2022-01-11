module Registry.Codec where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (bimap, lmap)
import Data.Codec (basicCodec, decode, encode)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as CA
import Data.Either (Either, note)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.NonEmpty.Internal (NonEmptyString)
import Data.String.NonEmpty.Internal as NES

-- | Codec for newtyped strings where the error message
-- | includes the constructor's name
newtypeStringCodec :: forall a. Newtype a String => String -> JsonCodec a
newtypeStringCodec ctorName = basicCodec dec enc
  where
  enc = encode CA.string <<< unwrap
  dec = bimap (Named ctorName) wrap <<< decode CA.string

neArray :: forall a. JsonCodec a -> JsonCodec (NonEmptyArray a)
neArray elemCodec = basicCodec dec enc
  where
  enc = encode (CA.array elemCodec) <<< NEA.toArray
  dec j = do
    arr <- lmap (Named "NonEmptyArray") $ decode (CA.array elemCodec) j
    note (Named "NonEmptyArray" $ UnexpectedValue j)
      $ NEA.fromArray arr

neString :: JsonCodec NonEmptyString
neString = basicCodec dec enc
  where
  enc = encode CA.string <<< NES.toString
  dec j = do
    s <- decode CA.string j
    note (Named "NonEmptyString" $ UnexpectedValue j) $ NES.fromString s

-- | Attempt to parse a string as `Json`, failing with a typed error if the
-- | JSON string is malformed.
-- Needed because it's not defined in `purescript-codec-argonaut`
parseJson :: String -> Either JsonDecodeError Json
parseJson = lmap (\_ -> TypeMismatch "JSON") <<< jsonParser