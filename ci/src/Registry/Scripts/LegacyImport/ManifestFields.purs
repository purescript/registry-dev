module Registry.Scripts.LegacyImport.ManifestFields where

import Registry.Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as Compat
import Data.Codec.Argonaut.Record as CAR
import Data.String.NonEmpty (NonEmptyString)
import Registry.Codec (neArray, neString)

type ManifestFields =
  { license :: Maybe (NonEmptyArray NonEmptyString)
  , description :: Maybe String
  , dependencies :: Object String
  , devDependencies :: Object String
  }

manifestFieldsCodec :: JsonCodec ManifestFields
manifestFieldsCodec = CAR.object "ManifestFields"
  { license: Compat.maybe (neArray neString)
  , description: Compat.maybe CA.string
  , dependencies: Compat.foreignObject CA.string
  , devDependencies: Compat.foreignObject CA.string
  }

-- derive newtype instance Json.EncodeJson Bowerfile

-- instance Json.DecodeJson Bowerfile where
--   decodeJson json = do
--     obj <- Json.decodeJson json
--     description <- obj .:? "description"
--     license <- decodeStringOrStringArray obj "license"
--     dependencies <- fromMaybe mempty <$> obj .:? "dependencies"
--     devDependencies <- fromMaybe mempty <$> obj .:? "devDependencies"
--     pure $ Bowerfile { description, license, dependencies, devDependencies }

-- decodeStringOrStringArray :: JsonCodec (Maybe (NonEmptyArray NonEmptyString))
-- decodeStringOrStringArray = basicCodec dec enc
--   where
--   enc = encode (Compat.maybe (neArray neString))
--   dec j = do
--     value <- decode (Compat.maybe (neString <|> ))

--   decNEString = decode neString
--   decArrNeString = map Array.cat

-- decodeStringOrStringArray
--   :: Object Json
--   -> String
--   -> Either Json.JsonDecodeError (Maybe (NonEmptyArray NonEmptyString))
-- decodeStringOrStringArray obj fieldName = do
--   let typeError = const $ Json.AtKey fieldName $ Json.TypeMismatch "String or Array"
--   lmap typeError do
--     value <- obj .:? fieldName
--     case value of
--       Nothing -> pure Nothing
--       Just v -> do
--         decoded <- (Json.decodeJson v <#> Array.singleton) <|> Json.decodeJson v
--         pure $ NEA.fromArray $ Array.catMaybes $ map NES.fromString decoded
