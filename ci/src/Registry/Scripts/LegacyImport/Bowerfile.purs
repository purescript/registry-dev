module Registry.Scripts.LegacyImport.Bowerfile
  ( Bowerfile(..)
  , toManifestFields
  ) where

import Registry.Prelude

import Data.Argonaut (Json, (.:?))
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Registry.Scripts.LegacyImport.ManifestFields (ManifestFields)

toManifestFields :: Bowerfile -> ManifestFields
toManifestFields (Bowerfile fields) = fields

newtype Bowerfile = Bowerfile ManifestFields

derive newtype instance Eq Bowerfile
derive newtype instance Show Bowerfile
derive newtype instance Json.EncodeJson Bowerfile

instance Json.DecodeJson Bowerfile where
  decodeJson json = do
    obj <- Json.decodeJson json
    license <- decodeStringOrStringArray obj "license"
    dependencies <- fromMaybe mempty <$> obj .:? "dependencies"
    devDependencies <- fromMaybe mempty <$> obj .:? "devDependencies"
    pure $ Bowerfile { license, dependencies, devDependencies }

decodeStringOrStringArray
  :: Object Json
  -> String
  -> Either Json.JsonDecodeError (Maybe (NonEmptyArray NonEmptyString))
decodeStringOrStringArray obj fieldName = do
  let typeError = const $ Json.AtKey fieldName $ Json.TypeMismatch "String or Array"
  lmap typeError do
    value <- obj .:? fieldName
    case value of
      Nothing -> pure Nothing
      Just v -> do
        decoded <- (Json.decodeJson v <#> Array.singleton) <|> Json.decodeJson v
        pure $ NEA.fromArray $ Array.catMaybes $ map NES.fromString decoded
