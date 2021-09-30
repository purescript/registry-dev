module Foreign.SemVer
  ( SemVer
  , parseSemVer
  , printSemVer
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

import Data.Argonaut as Json
import Data.Function.Uncurried (Fn2, runFn2)
import Data.String as String

type SemVerJS =
  { major :: Int
  , minor :: Int
  , patch :: Int
  , prerelease :: Array String
  , build :: Array String
  , version :: String
  }

newtype SemVer = SemVer SemVerJS

instance eqSemVer :: Eq SemVer where
  eq v1 v2 = compare v1 v2 == EQ

instance ordSemVer :: Ord SemVer where
  compare = compareSemVer

instance showSemVer :: Show SemVer where
  show = printSemVer

instance decodeJsonSemver :: Json.DecodeJson SemVer where
  decodeJson json = do
    version <- Json.decodeJson json
    note (Json.TypeMismatch $ "Expected version: " <> version) (parseSemVer version)

instance encodeJsonSemver :: Json.EncodeJson SemVer where
  encodeJson = Json.encodeJson <<< printSemVer

foreign import compareSemVerImpl :: Fn2 SemVerJS SemVerJS Int

compareSemVer :: SemVer -> SemVer -> Ordering
compareSemVer (SemVer v1) (SemVer v2) = case runFn2 compareSemVerImpl v1 v2 of
  (-1) -> LT
  0 -> EQ
  1 -> GT
  other -> unsafeCrashWith $ "Unknown ordering: " <> show other

foreign import parseSemVerImpl :: String -> Nullable SemVer

parseSemVer :: String -> Maybe SemVer
parseSemVer = toMaybe <<< parseSemVerImpl

major :: SemVer -> Int
major (SemVer v) = v.major

minor :: SemVer -> Int
minor (SemVer v) = v.minor

patch :: SemVer -> Int
patch (SemVer v) = v.patch

prerelease :: SemVer -> Array String
prerelease (SemVer v) = v.prerelease

build :: SemVer -> Array String
build (SemVer v) = v.build

printSemVer :: SemVer -> String
printSemVer (SemVer v) = v.version

newtype Range = Range String

derive newtype instance eqRange :: Eq Range

instance decodeJsonRange :: Json.DecodeJson Range where
  decodeJson json = do
    original <- Json.decodeJson json
    note (Json.TypeMismatch $ "Expected SemVer range: " <> original) (parseRange original)

instance encodeJsonRange :: Json.EncodeJson Range where
  encodeJson = Json.encodeJson <<< printRange

foreign import parseRangeImpl :: String -> Nullable String

parseRange :: String -> Maybe Range
parseRange original = do
  converted <- case toMaybe (parseRangeImpl original) of
    Just c -> pure c
    Nothing ->
      -- when parsing a version from a BowerFile it could be that it's specified
      -- in the https://giturl#version, or owner/repo#version, so we try to parse that here.
      case String.split (String.Pattern "#") original of
        [ _url, version ] -> case toMaybe (parseRangeImpl version) of
          Just c -> pure c
          _ -> Nothing
        _ -> Nothing
  pure $ Range converted

printRange :: Range -> String
printRange (Range r) = r
