module Registry.Internal.Codec
  ( iso8601Date
  , iso8601DateTime
  , packageMap
  , versionMap
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.DateTime (Date, DateTime)
import Data.DateTime as DateTime
import Data.Either (Either)
import Data.Either as Either
import Data.FoldableWithIndex (forWithIndex_)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Formatter.DateTime as Formatter.Datetime
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Foreign.Object.ST as Object.ST
import Registry.Internal.Format as Internal.Format
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Registry.Version as Version

-- | INTERNAL
-- |
-- | A codec for date times that encode as JSON strings in the ISO8601 date-time
-- | format described here:
-- | https://tc39.es/ecma262/#sec-date-time-string-format
-- |
-- | WARNING: This codec is not bi-directional, because the input string may not
-- | be a valid simple ISO8601 string (ie. it may be an outdated metadata file
-- | using the RFC3339String format). The string will be modified if you read
-- | and then write.
iso8601DateTime :: JsonCodec DateTime
iso8601DateTime = Codec.codec' decode encode
  where
  encode :: DateTime -> Json
  encode =
    Formatter.DateTime.format Internal.Format.iso8601DateTime
      >>> Codec.encode CA.string

  decode :: Json -> Either CA.JsonDecodeError DateTime
  decode json = do
    string <- Codec.decode CA.string json
    let fixed = Internal.Format.rfc3339ToISO8601 string
    lmap (CA.TypeMismatch <<< append "ISO8601: ") (Formatter.Datetime.unformat Internal.Format.iso8601DateTime fixed)

-- | INTERNAL
-- |
-- | A codec for date times that encode as JSON strings in the ISO8601 date
-- | format, ie. YYYY-MM-DD
iso8601Date :: JsonCodec Date
iso8601Date = Codec.codec' decode encode
  where
  encode :: Date -> Json
  encode =
    flip DateTime.DateTime bottom
      >>> Formatter.DateTime.format Internal.Format.iso8601Date
      >>> Codec.encode CA.string

  decode :: Json -> Either CA.JsonDecodeError Date
  decode json = do
    string <- Codec.decode CA.string json
    dateTime <- lmap (CA.TypeMismatch <<< append "YYYY-MM-DD: ") (Formatter.DateTime.unformat Internal.Format.iso8601Date string)
    pure $ DateTime.date dateTime

-- | INTERNAL
-- |
-- | A codec for `Map` structures that have `PackageName`s as keys. Encodes as
-- | a JSON object, ie. `{ "aff": <value>, "argonaut": <value> }`
packageMap :: forall a. JsonCodec a -> JsonCodec (Map PackageName a)
packageMap = strMap "PackageName" (Either.hush <<< PackageName.parse) PackageName.print

-- | INTERNAL
-- |
-- | A codec for `Map` structures that have `Version`s as keys. Encodes as a
-- | JSON object, ie. `{ "1.0.0": <value>, "2.0.0": <value> }`
versionMap :: forall a. JsonCodec a -> JsonCodec (Map Version a)
versionMap = strMap "Version" (Either.hush <<< Version.parse) Version.print

-- | INTERNAL
-- |
-- | A codec for `Map` structures that have keys that can be encoded as strings.
-- | Represented as an object in JSON.
strMap :: forall k a. Ord k => String -> (String -> Maybe k) -> (k -> String) -> JsonCodec a -> JsonCodec (Map k a)
strMap type_ parse print valueCodec = Codec.codec' decode encode
  where
  encode :: Map k a -> Json
  encode m = Codec.encode CA.jobject $ Object.runST do
    obj <- Object.ST.new
    forWithIndex_ m \k v -> Object.ST.poke (print k) (Codec.encode valueCodec v) obj
    pure obj

  decode :: Json -> Either CA.JsonDecodeError (Map k a)
  decode json = do
    array :: Array _ <- Object.toUnfoldable <$> Codec.decode CA.jobject json
    parsed <- for array \(Tuple k v) -> do
      key <- Either.note (CA.AtKey k (CA.TypeMismatch type_)) (parse k)
      val <- lmap (CA.AtKey k) (Codec.decode valueCodec v)
      pure $ Tuple key val
    pure $ Map.fromFoldable parsed
