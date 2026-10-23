module Registry.App.Version
  ( parseLenient
  ) where

import Registry.App.Prelude

import Data.Int as Int
import Data.String as String
import Registry.Version as Version

parseLenient :: String -> Either String Version
parseLenient input = do
  String.trim input
    # maybeIdentity (String.stripPrefix (String.Pattern "v"))
    # String.split (String.Pattern ".")
    # map (maybeIdentity dropLeadingZeros)
    # String.joinWith "."
    # Version.parse
  where
  maybeIdentity k x = fromMaybe x (k x)
  dropLeadingZeros = map (Int.toStringAs Int.decimal) <<< Int.fromString
