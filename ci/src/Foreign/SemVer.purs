module Foreign.SemVer
  ( SemVer
  , parseSemVer
  , raw
  , version
  , major
  , minor
  , patch
  , prerelease
  , build
  , Range
  , parseRange
  , printRange
  ) where

import Registry.Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.String as String
import Registry.Json (class RegistryJson)
import Registry.Json as Json

data SemVer

instance eqSemVer :: Eq SemVer where
  eq v1 v2 = compare v1 v2 == EQ

instance ordSemVer :: Ord SemVer where
  compare = compareSemVer

instance showSemVer :: Show SemVer where
  show = raw

instance RegistryJson SemVer where
  encode = Json.encode <<< version
  decode json = do
    versionString <- Json.decode json
    note ("Expected version: " <> versionString) (parseSemVer versionString)

foreign import compareSemVerImpl :: Fn2 SemVer SemVer Int

compareSemVer :: SemVer -> SemVer -> Ordering
compareSemVer v1 v2 = case runFn2 compareSemVerImpl v1 v2 of
  (-1) -> LT
  0 -> EQ
  1 -> GT
  other -> unsafeCrashWith $ "Unknown ordering: " <> show other

foreign import parseSemVerImpl :: String -> Nullable SemVer

parseSemVer :: String -> Maybe SemVer
parseSemVer = toMaybe <<< parseSemVerImpl

foreign import major :: SemVer -> Int

foreign import minor :: SemVer -> Int

foreign import patch :: SemVer -> Int

foreign import prerelease :: SemVer -> Array String

foreign import build :: SemVer -> Array String

foreign import version :: SemVer -> String

foreign import raw :: SemVer -> String

newtype Range = Range String

derive newtype instance eqRange :: Eq Range

instance RegistryJson Range where
  encode = Json.encode <<< printRange
  decode json = do
    original <- Json.decode json
    note ("Expected SemVer range: " <> original) (parseRange original)

foreign import parseRangeImpl :: String -> Nullable String

parseRange :: String -> Maybe Range
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
  pure $ Range converted

printRange :: Range -> String
printRange (Range r) = r

instance Show Range where
  show = printRange
