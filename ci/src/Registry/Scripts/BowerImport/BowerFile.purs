module Registry.Scripts.BowerImport.BowerFile
  ( BowerFile
  , BowerFileParseError
  , printBowerFileParseError
  , parse
  , version
  , license
  , dependencies
  , devDependencies
  ) where

import Registry.Prelude

import Data.Argonaut (Json, (.:), (.:?))
import Data.Argonaut as Json
import Data.Array as Array
import Foreign.Jsonic as Jsonic

newtype BowerFile =
  BowerFile
    { version :: String
    , license :: Array String
    , dependencies :: Object String
    , devDependencies :: Object String
    }

derive newtype instance Show BowerFile
derive newtype instance Json.DecodeJson BowerFile
derive newtype instance Json.EncodeJson BowerFile

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
    >=> parseBowerFile

parseBowerFile :: Json -> Either BowerFileParseError BowerFile
parseBowerFile json = lmap JsonDecodeError do
  let
    strings :: Json -> Either Json.JsonDecodeError (Array String)
    strings = Json.caseJson
      (const $ Left $ Json.TypeMismatch "expected string or array")
      (const $ Left $ Json.TypeMismatch "expected string or array")
      (const $ Left $ Json.TypeMismatch "expected string or array")
      (Array.singleton >>> Right)
      (traverse Json.decodeJson)
      (const $ Left $ Json.TypeMismatch "expected string or array")
  root <- Json.decodeJson json
  version' <- root .: "version"
  license' <- strings =<< root .: "license"
  dependencies' <- fromMaybe mempty <$> root .:? "dependencies"
  devDependencies' <- fromMaybe mempty <$> root .:? "devDependencies"
  pure $
    BowerFile
      { version: version'
      , license: license'
      , dependencies: dependencies'
      , devDependencies: devDependencies'
      }

version :: BowerFile -> String
version (BowerFile r) = r.version

license :: BowerFile -> Array String
license (BowerFile r) = r.license

dependencies :: BowerFile -> Object String
dependencies (BowerFile r) = r.dependencies

devDependencies :: BowerFile -> Object String
devDependencies (BowerFile r) = r.devDependencies
