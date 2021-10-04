module Registry.Scripts.BowerImport.BowerFile
  ( BowerFile(..)
  , toManifestFields
  ) where

import Registry.Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, (.:?))
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Registry.Scripts.BowerImport.ManifestFields (ManifestFields)

toManifestFields :: BowerFile -> ManifestFields
toManifestFields (BowerFile fields) = fields

newtype BowerFile = BowerFile ManifestFields

derive newtype instance Eq BowerFile
derive newtype instance Show BowerFile
derive newtype instance Json.EncodeJson BowerFile

instance Json.DecodeJson BowerFile where
  decodeJson json = do
    obj <- Json.decodeJson json
    license <- decodeStringOrStringArray obj "license"
    dependencies <- fromMaybe mempty <$> obj .:? "dependencies"
    devDependencies <- fromMaybe mempty <$> obj .:? "devDependencies"
    pure $ BowerFile { license, dependencies, devDependencies }

decodeStringOrStringArray
  :: Object Json
  -> String
  -> Either Json.JsonDecodeError (Maybe (NonEmptyArray String))
decodeStringOrStringArray obj fieldName = do
  let typeError = const $ Json.AtKey fieldName $ Json.TypeMismatch "String or Array"
  lmap typeError do
    value <- obj .:? fieldName
    case value of
      Nothing -> pure Nothing
      Just v -> do
        decoded <- (Json.decodeJson v <#> Array.singleton) <|> Json.decodeJson v
        pure $ NEA.fromArray decoded
