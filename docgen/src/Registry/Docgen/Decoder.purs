module Registry.Docgen.Decoder where

import Prelude

import Codec.JSON.DecodeError (DecodeError)
import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Except (Except, catchError, runExcept, throwError)
import Data.Array as Array
import Data.Codec (Codec')
import Data.Codec as Codec
import Data.Either (Either, either)
import Data.Lazy as Lazy
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Profunctor.Star (Star(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import JSON (JObject, JSON)
import JSON as JSON
import JSON.Object as JObject
import JSON.Path as JP
import Safe.Coerce (coerce)

type Decoder a b = Star (Except DecodeError) a b

defer :: forall a b. (Unit -> Decoder a b) -> Decoder a b
defer k = Star \a -> do
  let (Star k') = Lazy.force lazy
  k' a
  where
  lazy = Lazy.defer k

throw :: forall a b. DecodeError -> Decoder a b
throw err = Star \_ -> throwError err

liftEither :: forall a b. (a -> Either String b) -> Decoder a b
liftEither fn = Star (either (throwError <<< DecodeError.basic) pure <<< fn)

runDecoder :: forall a b. Decoder a b -> a -> Either DecodeError b
runDecoder (Star k) = runExcept <<< k

fromCodec :: forall a b. Codec' (Except DecodeError) a b -> Decoder a b
fromCodec = Star <<< Codec.decode

toCodec :: forall a b. Decoder a b -> (b -> a) -> Codec' (Except DecodeError) a b
toCodec (Star decoder) = Codec.codec' decoder

decodeProp :: forall a. String -> Decoder JSON a -> Decoder JObject a
decodeProp prop (Star f) = Star \obj ->
  case JObject.lookup prop obj of
    Nothing ->
      throwError $ DecodeError.noValueFound (JP.AtKey prop JP.Tip)
    Just value ->
      catchError (f value) \err ->
        throwError $ DecodeError.withPath (JP.AtKey prop) err

decodePropOptional :: forall a. String -> Decoder JSON a -> Decoder JObject (Maybe a)
decodePropOptional prop (Star f) = Star \obj ->
  case JObject.lookup prop obj of
    Nothing ->
      pure Nothing
    Just value
      | JSON.isNull value ->
          pure Nothing
      | otherwise ->
          catchError (Just <$> f value) \err ->
            throwError $ DecodeError.withPath (JP.AtKey prop) err

decodeOptional :: forall a. Decoder JSON a -> Decoder JSON (Maybe a)
decodeOptional (Star f) = Star \json ->
  if JSON.isNull json then
    pure Nothing
  else
    Just <$> f json

decodeIndex :: forall a. Int -> Decoder JSON a -> Decoder (Array JSON) a
decodeIndex ix (Star f) = Star \arr ->
  case Array.index arr ix of
    Nothing ->
      throwError $ DecodeError.noValueFound (JP.AtIndex ix JP.Tip)
    Just value ->
      catchError (f value) \err ->
        throwError $ DecodeError.withPath (JP.AtIndex ix) err

decodeJObject :: forall a. Decoder JObject a -> Decoder JSON a
decodeJObject (Star f) = Star \json ->
  case JSON.toJObject json of
    Nothing ->
      throwError $ DecodeError.basic "Expected Object"
    Just obj ->
      f obj

decodeMap :: forall k v. Ord k => (String -> Decoder JSON (Tuple k v)) -> Decoder JObject (Map k v)
decodeMap k = Star \obj ->
  Map.fromFoldable <$> traverse
    ( \(Tuple key value) ->
        catchError (coerce (k key) value) \err ->
          throwError $ DecodeError.withPath (JP.AtKey key) err
    )
    (JObject.entries obj)

decodeArray :: forall a. Decoder (Array JSON) a -> Decoder JSON a
decodeArray (Star f) = Star \json ->
  case JSON.toArray json of
    Nothing ->
      throwError $ DecodeError.basic "Expected Array"
    Just arr ->
      f arr

traversedAtIndex :: forall f a. TraversableWithIndex Int f => Decoder JSON a -> Decoder (f JSON) (f a)
traversedAtIndex (Star f) = Star $ traverseWithIndex \ix value ->
  catchError (f value) \err ->
    throwError $ DecodeError.withPath (JP.AtIndex ix) err

guardLength :: Int -> Decoder (Array JSON) Unit
guardLength n = Star \arr ->
  if Array.length arr == n then
    pure unit
  else
    throwError $ DecodeError.basic $ "Exected Array of length " <> show n

decodeString :: Decoder JSON String
decodeString = Star \json ->
  case JSON.toString json of
    Nothing ->
      throwError $ DecodeError.basic "Expected String"
    Just str ->
      pure str

decodeInt :: Decoder JSON Int
decodeInt = Star \json ->
  case JSON.toInt json of
    Nothing ->
      throwError $ DecodeError.basic "Expected Int"
    Just int ->
      pure int

decodeNull :: Decoder JSON Unit
decodeNull = Star \json ->
  if JSON.isNull json then
    pure unit
  else
    throwError $ DecodeError.basic "Expected null"
