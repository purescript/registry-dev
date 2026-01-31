module Registry.Internal.Codec
  ( formatIso8601
  , iso8601Date
  , iso8601DateTime
  , limitedString
  , packageMap
  , parsedString
  , versionMap
  , strMap
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Except (Except, except, withExcept)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.DateTime (Date, DateTime)
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Formatter.DateTime as Formatter.Datetime
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Foreign.Object.ST as Object.ST
import JSON (JSON)
import JSON.Object as JSON.Object
import JSON.Path as JSON.Path
import Parsing (Parser)
import Parsing as Parsing
import Registry.Internal.Format as Internal.Format
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Registry.Version as Version

-- | INTERNAL
-- |
-- | Format a DateTime as an ISO8601 string.
formatIso8601 :: DateTime -> String
formatIso8601 = Formatter.DateTime.format Internal.Format.iso8601DateTime

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
iso8601DateTime :: CJ.Codec DateTime
iso8601DateTime = Codec.codec' decode encode
  where
  encode :: DateTime -> JSON
  encode =
    Formatter.DateTime.format Internal.Format.iso8601DateTime
      >>> CJ.encode CJ.string

  decode :: JSON -> Except CJ.DecodeError DateTime
  decode json = do
    string <- Codec.decode CJ.string json
    except case Internal.Format.rfc3339ToISO8601 string of
      Left err -> Left $ CJ.DecodeError.basic $ "Unable to parse input as ISO8601: " <> err
      Right fixed ->
        lmap (CJ.DecodeError.basic <<< append "ISO8601: ") (Formatter.Datetime.unformat Internal.Format.iso8601DateTime fixed)

-- | INTERNAL
-- |
-- | A codec for date times that encode as JSON strings in the ISO8601 date
-- | format, ie. YYYY-MM-DD
iso8601Date :: CJ.Codec Date
iso8601Date = Codec.codec' decode encode
  where
  encode :: Date -> JSON
  encode =
    flip DateTime.DateTime bottom
      >>> Formatter.DateTime.format Internal.Format.iso8601Date
      >>> CJ.encode CJ.string

  decode :: JSON -> Except CJ.DecodeError Date
  decode json = do
    string <- Codec.decode CJ.string json
    dateTime <- except $ lmap (CJ.DecodeError.basic <<< append "YYYY-MM-DD: ") (Formatter.DateTime.unformat Internal.Format.iso8601Date string)
    pure $ DateTime.date dateTime

-- | INTERNAL
-- |
-- | A codec for `String` values with an explicit limited length.
limitedString :: Int -> CJ.Codec String
limitedString limit = Codec.codec' decode encode
  where
  encode :: String -> JSON
  encode = CJ.encode CJ.string

  decode :: JSON -> Except CJ.DecodeError String
  decode json = except do
    string <- CJ.decode CJ.string json
    if String.length string > limit then
      Left $ CJ.DecodeError.basic $ "LimitedString: Exceeds limit of " <> Int.toStringAs Int.decimal limit <> " characters."
    else
      Right string

-- | INTERNAL
-- |
-- | A codec for `String` values that can be parsed into a `String`, failing
-- | with the parse error message if invalid.
parsedString :: Parser String String -> CJ.Codec String
parsedString parser = Codec.codec' decode encode
  where
  encode :: String -> JSON
  encode = CJ.encode CJ.string

  decode :: JSON -> Except CJ.DecodeError String
  decode json = except do
    string <- CJ.decode CJ.string json
    case Parsing.runParser string parser of
      Left error -> Left $ CJ.DecodeError.basic $ Parsing.parseErrorMessage error
      Right value -> pure value

-- | INTERNAL
-- |
-- | A codec for `Map` structures that have `PackageName`s as keys. Encodes as
-- | a JSON object, ie. `{ "aff": <value>, "argonaut": <value> }`
packageMap :: forall a. CJ.Codec a -> CJ.Codec (Map PackageName a)
packageMap = strMap "PackageName" PackageName.parse PackageName.print

-- | INTERNAL
-- |
-- | A codec for `Map` structures that have `Version`s as keys. Encodes as a
-- | JSON object, ie. `{ "1.0.0": <value>, "2.0.0": <value> }`
versionMap :: forall a. CJ.Codec a -> CJ.Codec (Map Version a)
versionMap = strMap "Version" Version.parse Version.print

-- | INTERNAL
-- |
-- | A codec for `Map` structures that have keys that can be encoded as strings.
-- | Represented as an object in JSON.
strMap :: forall k a. Ord k => String -> (String -> Either String k) -> (k -> String) -> CJ.Codec a -> CJ.Codec (Map k a)
strMap typeName parse print valueCodec = Codec.codec' decode encode
  where
  encode :: Map k a -> JSON
  encode m = CJ.encode CJ.jobject $ JSON.Object.fromFoldableWithIndex $ Object.runST do
    obj <- Object.ST.new
    forWithIndex_ m \k v -> Object.ST.poke (print k) (CJ.encode valueCodec v) obj
    pure obj

  decode :: JSON -> Except CJ.DecodeError (Map k a)
  decode json = do
    array :: Array _ <- JSON.Object.toUnfoldable <$> Codec.decode CJ.jobject json
    parsed <- for array \(Tuple k v) -> do
      key <- except $ lmap
        (CJ.DecodeError.error (JSON.Path.AtKey k JSON.Path.Tip) <<< append (typeName <> ": "))
        (parse k)
      val <- withExcept
        (CJ.DecodeError.withPath (\p -> JSON.Path.extend p (JSON.Path.AtKey k JSON.Path.Tip)))
        (Codec.decode valueCodec v)
      pure $ Tuple key val
    pure $ Map.fromFoldable parsed
