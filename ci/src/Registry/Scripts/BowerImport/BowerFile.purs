module Registry.Scripts.BowerImport.BowerFile
  ( BowerFile
  , BowerFileParseError(..)
  , parse
  , version
  , dependencies
  , devDependencies
  ) where

import Registry.Prelude

import Data.Argonaut (Json, (.:), (.:?))
import Data.Argonaut as Json
import Data.Generic.Rep (class Generic)
import Foreign.Jsonic as Jsonic

newtype BowerFile =
  BowerFile
    { version :: String
    , dependencies :: Object String
    , devDependencies :: Object String
    }

derive newtype instance Show BowerFile

data BowerFileParseError
  = JsonDecodeError Json.JsonDecodeError
  | JsonParseError String

derive instance Eq BowerFileParseError
derive instance Generic BowerFileParseError _

instance Show BowerFileParseError where
  show = genericShow

parse :: String -> Either BowerFileParseError BowerFile
parse =
  Jsonic.parse
    >>> lmap JsonParseError
    >=> parseBowerFile

parseBowerFile :: Json -> Either BowerFileParseError BowerFile
parseBowerFile json = lmap JsonDecodeError do
  root <- Json.decodeJson json
  version' <- root .: "version"
  dependencies' <- fromMaybe mempty <$> root .:? "dependencies"
  devDependencies' <- fromMaybe mempty <$> root .:? "devDependencies"
  pure $
    BowerFile
      { version: version'
      , dependencies: dependencies'
      , devDependencies: devDependencies'
      }

version :: BowerFile -> String
version (BowerFile r) = r.version

dependencies :: BowerFile -> Object String
dependencies (BowerFile r) = r.dependencies

devDependencies :: BowerFile -> Object String
devDependencies (BowerFile r) = r.devDependencies
