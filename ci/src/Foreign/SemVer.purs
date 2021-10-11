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

data SemVerJS

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

foreign import majorImpl :: SemVerJS -> Int
major :: SemVer -> Int
major (SemVer v) = majorImpl v

foreign import minorImpl :: SemVerJS -> Int
minor :: SemVer -> Int
minor (SemVer v) = minorImpl v

foreign import patchImpl :: SemVerJS -> Int
patch :: SemVer -> Int
patch (SemVer v) = patchImpl v

foreign import prereleaseImpl :: SemVerJS -> Array String
prerelease :: SemVer -> Array String
prerelease (SemVer v) = prereleaseImpl v

foreign import buildImpl :: SemVerJS -> Array String
build :: SemVer -> Array String
build (SemVer v) = buildImpl v

foreign import versionImpl :: SemVerJS -> String
printSemVer :: SemVer -> String
printSemVer (SemVer v) = versionImpl v

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
      -- when parsing a version from a Bowerfile it could be that it's specified
      -- in the https://giturl#version, or owner/repo#version, so we try to parse that here.
      case String.split (String.Pattern "#") original of
        [ _url, version ] -> case toMaybe (parseRangeImpl version) of
          Just c -> pure c
          _ -> Nothing
        _ -> Nothing
  pure $ Range converted

printRange :: Range -> String
printRange (Range r) = r
