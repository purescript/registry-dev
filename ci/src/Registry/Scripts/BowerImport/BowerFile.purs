module Registry.Scripts.BowerImport.BowerFile (BowerFile, BowerFileParseError, parse) where

import Registry.Prelude

import Data.Argonaut (Json, (.:), (.:?))
import Data.Argonaut as Json
import Data.Map as Map
import Data.Maybe (maybe)
import Foreign.Jsonic as Jsonic
import Foreign.SemVer (SemVer)

newtype BowerFile =
  BowerFile
    { name :: String -- Does this need to be a packageName type?
    , version :: SemVer
    , dependencies :: Map String SemVer -- Same question as above
    , devDependencies :: Map String SemVer -- Same question as above
    }

derive newtype instance Show BowerFile

data BowerFileParseError
  = GenericError
  | JsonDecodeError Json.JsonDecodeError
  | JsonParseError String

derive instance Eq BowerFileParseError

instance Show BowerFileParseError where
  show = case _ of
    GenericError -> "GenericError"
    JsonDecodeError de -> "JsonDecodeError: " <> Json.printJsonDecodeError de
    JsonParseError s -> "JsonParseError: " <> s

parse :: String -> Either BowerFileParseError BowerFile
parse =
  Jsonic.parse
    >>> lmap JsonParseError
    >=> parseBowerFile

parseBowerFile :: Json -> Either BowerFileParseError BowerFile
parseBowerFile json = lmap JsonDecodeError do
  root <- Json.decodeJson json
  name <- root .: "name"
  version <- root .: "version"
  dependencies <- objectToStringMap <$> root .:? "dependencies"
  devDependencies <- objectToStringMap <$> root .:? "devDependencies"
  pure $ BowerFile { name, version, dependencies, devDependencies }

objectToStringMap :: forall a. Maybe (Object a) -> Map String a
objectToStringMap = maybe Map.empty $ objectToMap pure

