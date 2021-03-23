module SemVer
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

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Partial.Unsafe (unsafeCrashWith)

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

instance decodeJsonSemver :: DecodeJson SemVer where
  decodeJson json = do
    version <- decodeJson json
    note (TypeMismatch $ "Expected version: " <> version) (parseSemVer version)

instance encodeJsonSemver :: EncodeJson SemVer where
  encodeJson = encodeJson <<< printSemVer

foreign import compareSemVerImpl :: SemVerJS -> SemVerJS -> Int
compareSemVer :: SemVer -> SemVer -> Ordering
compareSemVer (SemVer v1) (SemVer v2) = case compareSemVerImpl v1 v2 of
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

foreign import parseRangeImpl :: String -> Nullable String
parseRange :: String -> Maybe Range
parseRange = map Range <<< toMaybe <<< parseRangeImpl

printRange :: Range -> String
printRange (Range r) = r