module Registry.Scripts.LegacyImport.Bowerfile (Bowerfile(..)) where

import Registry.Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, (.:?))
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty as NEA

-- TODO: Just make this an instance on top of `ManifestFields`? Make it so that
-- you can encode / decode a `Bowerfile` or a `SpagoJson`, but underneath they
-- both easily transform to a `ManifestFields` type?
newtype Bowerfile = Bowerfile
  { license :: Maybe (NonEmptyArray String)
  , dependencies :: Object String
  , devDependencies :: Object String
  }

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
