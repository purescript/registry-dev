module Registry.App.LenientVersion
  ( LenientVersion(..)
  , parse
  , parser
  , print
  , raw
  , version
  ) where

import Registry.App.Prelude

import Control.Monad.Error.Class as Error
import Data.Function (on)
import Data.Int as Int
import Data.String as String
import Parsing (Parser)
import Parsing as Parsing
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
  -- First we ensure there are no leading or trailing spaces.
  String.trim input
    -- Then we remove a 'v' prefix, if present.
    # maybeIdentity (String.stripPrefix (String.Pattern "v"))
    -- Then we group by where the version digits ought to be...
    # String.split (String.Pattern ".")
    -- ...so that we can trim any leading zeros
    # map (maybeIdentity dropLeadingZeros)
    -- and rejoin the string.
    # String.joinWith "."
  where
  maybeIdentity k x = fromMaybe x (k x)
  dropLeadingZeros = map (Int.toStringAs Int.decimal) <<< Int.fromString
