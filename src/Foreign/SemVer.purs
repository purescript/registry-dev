module Foreign.SemVer
  ( parseRange
  ) where

import Registry.Prelude

import Data.String as String

foreign import parseRangeImpl :: String -> Nullable String

parseRange :: String -> Maybe String
parseRange original = do
  converted <- case toMaybe (parseRangeImpl original) of
    Just c -> pure c
    Nothing ->
      -- when parsing a version from a Bowerfile it could be that it's specified
      -- in the https://giturl#version, or owner/repo#version, so we try to parse that here.
      case String.split (String.Pattern "#") original of
        [ _, v ] -> case toMaybe (parseRangeImpl v) of
          Just c -> pure c
          _ -> Nothing
        _ -> Nothing
  pure converted
