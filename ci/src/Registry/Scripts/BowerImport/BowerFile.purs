module Registry.Scripts.BowerImport.BowerFile
  ( BowerFile(..)
  , BowerFileParseError
  , printBowerFileParseError
  , parse
  ) where

import Registry.Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, (.:), (.:?))
import Data.Argonaut as Json
import Data.Array as Array
import Foreign.Jsonic as Jsonic

newtype BowerFile = BowerFile
  { license :: Array String
  , dependencies :: Object String
  , devDependencies :: Object String
  }

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

decodeStringOrStringArray :: Object Json -> String -> Either Json.JsonDecodeError (Array String)
decodeStringOrStringArray obj fieldName = do
  let typeError = const $ Json.AtKey fieldName $ Json.TypeMismatch "String or Array"
  lmap typeError do
    value <- obj .: fieldName
    (Json.decodeJson value <#> Array.singleton) <|> Json.decodeJson value

data BowerFileParseError
  = JsonDecodeError Json.JsonDecodeError
  | JsonParseError String

derive instance Eq BowerFileParseError

printBowerFileParseError :: BowerFileParseError -> String
printBowerFileParseError = case _ of
  JsonDecodeError err -> "JsonDecodeError: " <> Json.printJsonDecodeError err
  JsonParseError err -> "JsonParseError: " <> err

instance Show BowerFileParseError where
  show = printBowerFileParseError

parse :: String -> Either BowerFileParseError BowerFile
parse =
  Jsonic.parse
    >>> lmap JsonParseError
    >=> (Json.decodeJson >>> lmap JsonDecodeError)
