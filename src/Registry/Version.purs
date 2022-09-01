module Registry.Version
  ( ParseMode(..)
  , Range
  , Version
  , bumpHighest
  , bumpMajor
  , bumpMinor
  , bumpPatch
  , greaterThanOrEq
  , intersect
  , lessThan
  , major
  , minor
  , parseRange
  , parseVersion
  , patch
  , printRange
  , printVersion
  , rangeIncludes
  , rawRange
  , rawVersion
  , versionParser
  ) where

import Registry.Prelude

import Control.Monad.Error.Class as Monad.Error
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (fromRight)
import Data.Function (on)
import Data.Int as Int
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.String.CodeUnits as String.CodeUnits
import Foreign.SemVer as SemVer
import Parsing (ParseError, Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic
import Registry.Json (class StringEncodable)
import Registry.Json as Json

-- | A Registry-compliant version of the form 'X.Y.Z', where each place is a
-- | non-negative integer.
newtype Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  , mode :: ParseMode
  , raw :: String
  }

instance Eq Version where
  eq = eq `on` (\(Version v) -> [ v.major, v.minor, v.patch ])

instance Ord Version where
  compare = compare `on` (\(Version v) -> [ v.major, v.minor, v.patch ])

instance Show Version where
  show = printVersion

instance StringEncodable Version where
  toEncodableString = printVersion
  fromEncodableString = lmap Parsing.parseErrorMessage <<< parseVersion Strict

instance RegistryJson Version where
  encode = Json.encode <<< Json.toEncodableString
  decode = Json.fromEncodableString <=< Json.decode

major :: Version -> Int
major (Version version) = version.major

minor :: Version -> Int
minor (Version version) = version.minor

patch :: Version -> Int
patch (Version version) = version.patch

rawVersion :: Version -> String
rawVersion (Version version) = version.raw

printVersion :: Version -> String
printVersion version = do
  let printInt = Int.toStringAs Int.decimal
  Array.fold
    [ printInt (major version)
    , "."
    , printInt (minor version)
    , "."
    , printInt (patch version)
    ]

parseVersion :: ParseMode -> String -> Either ParseError Version
parseVersion mode input = Parsing.runParser input (versionParser mode)

versionParser :: ParseMode -> Parser String Version
versionParser mode = do
  Parsing.ParseState input _ _ <- Parsing.getParserT
  -- We allow leading whitespace and the commonly-used 'v' character prefix in
  -- lenient mode.
  when (mode == Lenient) do
    _ <- Parsing.String.Basic.whiteSpace
    _ <- Parsing.Combinators.optional $ Parsing.String.char 'v'
    pure unit
  major' <- nonNegativeInt
  _ <- Parsing.String.char '.'
  minor' <- nonNegativeInt
  _ <- Parsing.String.char '.'
  patch' <- nonNegativeInt
  when (mode == Lenient) do
    _ <- Parsing.String.Basic.whiteSpace
    pure unit
  Parsing.String.eof
  pure $ Version { major: major', minor: minor', patch: patch', mode, raw: input }
  where
  nonNegativeInt :: Parser String Int
  nonNegativeInt = do
    digitChars <- Parsing.Combinators.Array.many1 Parsing.String.Basic.digit
    let
      zeroCount = Array.length $ NonEmptyArray.takeWhile (_ == '0') digitChars
      digitString = CodeUnits.fromCharArray $ NonEmptyArray.toArray digitChars
      failInteger = Parsing.fail $ "Invalid 32-bit integer: " <> digitString
    integer <- maybe failInteger pure $ Int.fromString digitString
    -- We do not accept leading zeros in versions unless we are in lenient mode
    when (mode == Strict && (zeroCount > 1 || (zeroCount == 1 && integer /= 0))) do
      Parsing.fail $ "Leading zeros are not allowed: " <> digitString
    when (integer < 0) do
      Parsing.fail $ "Invalid non-negative integer: " <> show integer
    pure integer

-- | A Registry-compliant version range of the form '>=X.Y.Z <X.Y.Z', where the
-- | left-hand version is less than the right-hand version.
newtype Range = Range
  { lhs :: Version
  , rhs :: Version
  , mode :: ParseMode
  , raw :: String
  }

instance Eq Range where
  eq = eq `on` (\(Range { lhs, rhs }) -> [ lhs, rhs ])

instance RegistryJson Range where
  encode = Json.encode <<< printRange
  decode json = do
    string <- Json.decode json
    lmap Parsing.parseErrorMessage $ parseRange Strict string

instance Show Range where
  show = printRange

greaterThanOrEq :: Range -> Version
greaterThanOrEq (Range range) = range.lhs

lessThan :: Range -> Version
lessThan (Range range) = range.rhs

-- | Check whether a range includes the provided version
rangeIncludes :: Range -> Version -> Boolean
rangeIncludes (Range { lhs, rhs }) version = version >= lhs && version < rhs

rawRange :: Range -> String
rawRange (Range range) = range.raw

printRange :: Range -> String
printRange range =
  Array.fold
    [ ">="
    , printVersion (greaterThanOrEq range)
    , " <"
    , printVersion (lessThan range)
    ]

intersect :: Range -> Range -> Maybe Range
intersect (Range r1) (Range r2)
  | r1.lhs >= r2.rhs || r2.lhs >= r1.rhs = Nothing
  | otherwise = Just $ Range
      { lhs: max r1.lhs r2.lhs
      , rhs: min r1.rhs r2.rhs
      , mode: Lenient
      , raw: r1.raw <> " && " <> r2.raw
      }

parseRange :: ParseMode -> String -> Either ParseError Range
parseRange mode input = do
  let
    parserInput :: String
    parserInput = case mode of
      Lenient -> convertRange input
      Strict -> input

  Parsing.runParser parserInput do
    _ <- Parsing.String.string ">=" <|> Parsing.fail "Ranges must begin with >="
    lhs <- toVersion mode =<< map String.CodeUnits.fromCharArray charsUntilSpace
    _ <- Parsing.String.char '<' <|> Parsing.fail "Ranges must end with <"
    rhs <- toVersion mode =<< map String.CodeUnits.fromCharArray chars
    -- Parsing.String.eof
    -- Trimming prerelease identifiers in lenient mode can produce ranges
    -- where the lhs was less than the rhs, but no longer is. For example:
    -- '>=1.0.0-rc.1 <1.0.0-rc.5' -> '>=1.0.0 <1.0.0'.
    -- We fix these ranges in lenient mode by bumping the rhs patch by one.
    if (mode == Lenient && lhs == rhs) then
      pure $ Range { lhs, rhs: bumpPatch rhs, mode, raw: input }
    else if (lhs >= rhs) then
      Parsing.fail $ Array.fold
        [ "Left-hand version ("
        , printVersion lhs
        , ") must be less than right-hand version ("
        , printVersion rhs
        , ")"
        ]
    else
      pure $ Range { lhs, rhs, mode, raw: input }

data ParseMode = Lenient | Strict

derive instance Eq ParseMode

-- In lenient mode we attempt to clean up ranges that are valid but use a syntax
-- other than one supported by the registry.
--
-- When only an upper bound is specified we can set the lower bound to 0.0.0
-- "<1.0.0" -> ">=0.0.0 <1.0.0", "<=0.1.1" -> ">=0.0.0 <0.1.0"
--
-- When ranges unexpectedly end with `<=` we can increment the patch version:
-- ">=1.0.0 <=2.0.0" -> ">=1.0.0 <2.0.1"
--
-- When ranges unexpectedly begin with `>` we can increment the patch version:
-- ">1.1.0 <2.0.0" -> ">=1.1.1 <2.0.0"
--
-- When ranges are exact, we can create a range:
-- "1.0.0" -> ">=1.0.0 <1.0.1"
--
-- When ranges contain prerelease identifiers or build metadata we can strip it
-- ">=1.0.0-rc.1 <2.0.0-0" -> ">=1.0.0 <2.0.0"
convertRange :: String -> String
convertRange input = fromRight input do
  -- The `parseRange` function converts most ranges into simple ranges that are
  -- likely to be accepted by the registry.
  semVer <- note "Could not parse with parseRange" $ SemVer.parseRange input

  pure do
    semVer
      # fixGtLhs
      # fixLtRhs
      # fixUpperOnly
      # fixExactVersions
  where
  -- Fix ranges that begin with '>' instead of '>='
  fixGtLhs :: String -> String
  fixGtLhs str = fromRight str $ Parsing.runParser str do
    _ <- Parsing.String.char '>'
    Parsing.Combinators.notFollowedBy (Parsing.String.char '=')
    lhsChars <- map String.CodeUnits.fromCharArray charsUntilSpace
    lhs <- toVersion Lenient lhsChars
    Parsing.ParseState suffix _ _ <- Parsing.getParserT
    pure $ Array.fold [ ">=", printVersion (bumpPatch lhs), " ", suffix ]

  -- Fix ranges that end with '<=' instead of '<'
  fixLtRhs str = fromRight str $ Parsing.runParser str do
    _ <- Parsing.String.string ">="
    lhs <- toVersion Lenient =<< map String.CodeUnits.fromCharArray charsUntilSpace
    _ <- Parsing.String.string "<="
    rhs <- toVersion Lenient =<< map String.CodeUnits.fromCharArray chars
    Parsing.String.eof
    pure $ Array.fold [ ">=", printVersion lhs, " <", printVersion (bumpPatch rhs) ]

  -- Fix ranges that only consist of an upper bound ('<1.0.0')
  fixUpperOnly str = fromRight str $ Parsing.runParser str do
    _ <- Parsing.String.char '<'
    hasEq <- Parsing.Combinators.optionMaybe (Parsing.String.char '=')
    lhs <- toVersion Lenient =<< map String.CodeUnits.fromCharArray chars
    Parsing.String.eof
    pure $ Array.fold [ ">=0.0.0 <", printVersion (if isJust hasEq then bumpPatch lhs else lhs) ]

  -- Replace exact ranges ('1.0.0') with ranges ('>=1.0.0 <1.0.1')
  fixExactVersions str = fromRight str do
    version <- parseVersion Lenient str
    pure $ Array.fold [ ">=", printVersion version, " <", printVersion (bumpPatch version) ]

toVersion :: ParseMode -> String -> Parser String Version
toVersion mode string = Monad.Error.liftEither case mode of
  Lenient -> do
    let truncate pattern input = fromMaybe input $ Array.head $ String.split pattern input
    let noPrerelease = truncate (String.Pattern "-")
    let noBuild = truncate (String.Pattern "+")
    Parsing.runParser (noPrerelease (noBuild string)) (versionParser Lenient)
  Strict ->
    Parsing.runParser string (versionParser Strict)

chars :: Parser String (Array Char)
chars = Array.many Parsing.String.anyChar

charsUntil :: forall a. Parser String a -> Parser String (Array Char)
charsUntil = map fst <<< Parsing.Combinators.Array.manyTill_ Parsing.String.anyChar

charsUntilSpace :: Parser String (Array Char)
charsUntilSpace = charsUntil (Parsing.String.char ' ')

-- | Increments the greatest SemVer position.
-- |   - 0.0.1 becomes 0.0.2
-- |   - 0.1.1 becomes 0.2.0
-- |   - 1.1.1 becomes 2.0.0
bumpHighest :: Version -> Version
bumpHighest version@(Version v) =
  if v.major >= 1 then bumpMajor version
  else if v.minor >= 1 then bumpMinor version
  else bumpPatch version

bumpMajor :: Version -> Version
bumpMajor (Version version) = Version (version { major = version.major + 1, minor = 0, patch = 0 })

bumpMinor :: Version -> Version
bumpMinor (Version version) = Version (version { minor = version.minor + 1, patch = 0 })

bumpPatch :: Version -> Version
bumpPatch (Version version) = Version (version { patch = version.patch + 1 })
