module Foreign.SemVer
  ( SemVer
  , parseSemVer
  , printSemVer
  , major
  , maxSatisfying
  , minor
  , patch
  , prerelease
  , build
  , Range
  , parseRange
  , printRange
  , satisfies
  ) where

import Registry.Prelude

import Data.Argonaut as Json
import Data.Function.Uncurried (Fn2, runFn2)
import Data.String as String

data SemVer

instance eqSemVer :: Eq SemVer where
  eq v1 v2 = compare v1 v2 == EQ

instance ordSemVer :: Ord SemVer where
  compare = compareSemVer

instance showSemVer :: Show SemVer where
  show = printSemVer

instance decodeJsonSemver :: Json.DecodeJson SemVer where
  decodeJson json = do
    version' <- Json.decodeJson json
    note (Json.TypeMismatch $ "Expected version: " <> version') (parseSemVer version')

instance encodeJsonSemver :: Json.EncodeJson SemVer where
  encodeJson = Json.encodeJson <<< printSemVer

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

printSemVer :: SemVer -> String
printSemVer = version

foreign import major :: SemVer -> Int

foreign import minor :: SemVer -> Int

foreign import patch :: SemVer -> Int

foreign import prerelease :: SemVer -> Array String

foreign import build :: SemVer -> Array String

foreign import version :: SemVer -> String

newtype Range = Range String

derive newtype instance eqRange :: Eq Range
derive newtype instance ordRange :: Ord Range

foreign import maxSatisfyingImpl :: Fn2 (Array String) Range (Nullable String)

maxSatisfying :: Array SemVer -> Range -> Maybe SemVer
maxSatisfying versions range = parseSemVer =<< toMaybe (runFn2 maxSatisfyingImpl (map version versions) range)

foreign import satisfiesImpl :: Fn2 String Range Boolean

satisfies :: SemVer -> Range -> Boolean
satisfies version' range = runFn2 satisfiesImpl (version version') range

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
        [ _, v ] -> case toMaybe (parseRangeImpl v) of
          Just c -> pure c
          _ -> Nothing
        _ -> Nothing
  pure $ Range converted

printRange :: Range -> String
printRange (Range r) = r
