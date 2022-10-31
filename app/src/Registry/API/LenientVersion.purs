module Registry.API.LenientVersion
  ( LenientVersion(..)
  , parse
  , parser
  , print
  , raw
  , version
  ) where

import Registry.Prelude

import Control.Monad.Error.Class as Error
import Data.Array as Array
import Data.Function (on)
import Data.String as String
import Parsing (Parser)
import Parsing as Parsing
import Registry.Json (class StringEncodable)
import Registry.Json as Json
import Registry.Version (Version)
import Registry.Version as Version

-- | A lenient variant on the `Registry.Version` type, which attempts to correct
-- | issues with the input string while parsing and retains the raw input.
newtype LenientVersion = LenientVersion
  { version :: Version
  , raw :: String
  }

derive instance Newtype LenientVersion _

instance Eq LenientVersion where
  eq = eq `on` (_.version <<< un LenientVersion)

instance Ord LenientVersion where
  compare = comparing (_.version <<< un LenientVersion)

instance StringEncodable LenientVersion where
  toEncodableString = raw
  fromEncodableString = parse

instance RegistryJson LenientVersion where
  encode = Json.encode <<< Json.toEncodableString
  decode = Json.fromEncodableString <=< Json.decode

-- | Print the `Version` within a lenient version
print :: LenientVersion -> String
print (LenientVersion v) = Version.print v.version

-- | Retrieve the parsed `Version` from a lenient version
version :: LenientVersion -> Version
version (LenientVersion v) = v.version

-- | Print the input string that was used to parse a lenient version
raw :: LenientVersion -> String
raw (LenientVersion v) = v.raw

-- | Parse a version in lenient mode, trimming spaces, prerelease identifiers,
-- | and build metadata as needed.
parse :: String -> Either String LenientVersion
parse = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser parser

parser :: Parser String LenientVersion
parser = do
  Parsing.ParseState unparsed _ _ <- Parsing.getParserT
  parsed <- Error.liftEither $ Parsing.runParser (convertVersion unparsed) Version.parser
  pure $ LenientVersion { raw: unparsed, version: parsed }

convertVersion :: String -> String
convertVersion input = do
  let truncate pattern str = fromMaybe str $ Array.head $ String.split pattern str
  let noPrerelease = truncate (String.Pattern "-")
  let noBuild = truncate (String.Pattern "+")
  let noV str = fromMaybe input $ String.stripPrefix (String.Pattern "v") str
  noPrerelease $ noBuild $ noV $ String.trim input
