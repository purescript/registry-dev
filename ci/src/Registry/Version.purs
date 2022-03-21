module Registry.Version
  ( Version
  , major
  , minor
  , patch
  , rawVersion
  , printVersion
  , parseVersion
  , Range
  , rangeIncludes
  , greaterThanOrEq
  , lessThan
  , printRange
  , rawRange
  , ParseMode(..)
  , parseRange
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.Either (fromRight)
import Data.Foldable (class Foldable)
import Data.Function (on)
import Data.Int as Int
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Foreign.SemVer as SemVer
import Registry.Json (class StringEncodable)
import Registry.Json as Json
import Text.Parsing.StringParser (ParseError, Parser)
import Text.Parsing.StringParser as StringParser
import Text.Parsing.StringParser.CodeUnits as StringParser.CodeUnits
import Text.Parsing.StringParser.Combinators as StringParser.Combinators

-- | A Registry-compliant version of the form 'X.Y.Z', where each place is a
-- | non-negative integer.
newtype Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  , mode :: ParseMode
  , raw :: String
  }

derive instance Eq Version

instance Ord Version where
  compare = compare `on` (\(Version v) -> [ v.major, v.minor, v.patch ])

instance Show Version where
  show = printVersion

instance StringEncodable Version where
  toEncodableString = printVersion
  fromEncodableString = lmap StringParser.printParserError <<< parseVersion Strict

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
parseVersion mode input = flip StringParser.runParser input do
  -- We allow leading whitespace and the commonly-used 'v' character prefix in
  -- lenient mode.
  when (mode == Lenient) do
    _ <- StringParser.CodeUnits.whiteSpace
    _ <- StringParser.Combinators.optional $ StringParser.CodeUnits.char 'v'
    pure unit
  major' <- nonNegativeInt
  _ <- StringParser.CodeUnits.char '.'
  minor' <- nonNegativeInt
  _ <- StringParser.CodeUnits.char '.'
  patch' <- nonNegativeInt
  when (mode == Lenient) do
    _ <- StringParser.CodeUnits.whiteSpace
    pure unit
  StringParser.CodeUnits.eof
  pure $ Version { major: major', minor: minor', patch: patch', mode, raw: input }
  where
  nonNegativeInt :: Parser Int
  nonNegativeInt = do
    digitChars <- StringParser.Combinators.many1 StringParser.CodeUnits.anyDigit
    let
      zeroCount = List.length $ NEL.takeWhile (_ == '0') digitChars
      digitString = CodeUnits.fromCharArray $ Array.fromFoldable digitChars
      failInteger = StringParser.fail $ "Invalid 32-bit integer: " <> digitString
    integer <- maybe failInteger pure $ Int.fromString digitString
    -- We do not accept leading zeros in versions unless we are in lenient mode
    when (mode == Strict && (zeroCount > 1 || (zeroCount == 1 && integer /= 0))) do
      StringParser.fail $ "Leading zeros are not allowed: " <> digitString
    when (integer < 0) do
      StringParser.fail $ "Invalid non-negative integer: " <> show integer
    pure integer

-- | A Registry-compliant version range of the form '>=X.Y.Z <X.Y.Z', where the
-- | left-hand version is less than the right-hand version.
newtype Range = Range
  { lhs :: Version
  , rhs :: Version
  , mode :: ParseMode
  , raw :: String
  }

derive instance Eq Range

instance RegistryJson Range where
  encode = Json.encode <<< printRange
  decode json = do
    string <- Json.decode json
    lmap StringParser.printParserError $ parseRange Strict string

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

parseRange :: ParseMode -> String -> Either ParseError Range
parseRange mode input = do
  let
    parserInput :: String
    parserInput = case mode of
      Lenient -> convertRange input
      Strict -> input

  parserInput # StringParser.runParser do
    _ <- StringParser.CodeUnits.string ">="
    lhs <- toVersion mode =<< map toString charsUntilSpace
    _ <- StringParser.CodeUnits.char '<'
    rhs <- toVersion mode =<< map toString chars
    StringParser.CodeUnits.eof
    -- Trimming prerelease identifiers in lenient mode can produce ranges
    -- where the lhs was less than the rhs, but no longer is. For example:
    -- '>=1.0.0-rc.1 <1.0.0-rc.5' -> '>=1.0.0 <1.0.0'.
    -- We fix these ranges in lenient mode by bumping the rhs patch by one.
    if (mode == Lenient && lhs == rhs) then
      pure $ Range { lhs, rhs: bumpPatch rhs, mode, raw: input }
    else if (lhs >= rhs) then
      StringParser.fail $ Array.fold
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
  fixGtLhs str = fromRight str do
    let
      parser = do
        _ <- StringParser.CodeUnits.char '>'
        lhs <- toVersion Lenient =<< map toString charsUntilSpace
        pure lhs

    StringParser.unParser parser { str, pos: 0 } <#> \{ result: version, suffix } ->
      ">=" <> printVersion (bumpPatch version) <> " " <> String.drop suffix.pos suffix.str

  -- Fix ranges that end with '<=' instead of '<'
  fixLtRhs str = fromRight str do
    let
      parser = do
        _ <- StringParser.CodeUnits.string ">="
        lhs <- toVersion Lenient =<< map toString charsUntilSpace
        _ <- StringParser.CodeUnits.string "<="
        rhs <- toVersion Lenient =<< map toString chars
        StringParser.CodeUnits.eof
        pure { lhs, rhs }

    StringParser.runParser parser str <#> \{ lhs, rhs } ->
      Array.fold
        [ ">="
        , printVersion lhs
        , " <"
        , printVersion (bumpPatch rhs)
        ]

  -- Fix ranges that only consist of an upper bound ('<1.0.0')
  fixUpperOnly str = fromRight str do
    let
      parser = do
        _ <- StringParser.CodeUnits.char '<'
        hasEq <- StringParser.Combinators.optionMaybe $ StringParser.CodeUnits.char '='
        lhs <- toVersion Lenient =<< map toString chars
        StringParser.CodeUnits.eof
        pure { lhs, hasEq: isJust hasEq }

    StringParser.runParser parser str <#> \{ lhs, hasEq } ->
      ">=0.0.0 <" <> printVersion (if hasEq then bumpPatch lhs else lhs)

  -- Replace exact ranges ('1.0.0') with ranges ('>=1.0.0 <1.0.1')
  fixExactVersions str = fromRight str do
    version <- parseVersion Lenient str
    pure $ Array.fold
      [ ">="
      , printVersion version
      , " <"
      , printVersion (bumpPatch version)
      ]

toVersion :: ParseMode -> String -> Parser Version
toVersion mode string = do
  let
    parsed = case mode of
      Lenient -> do
        let truncate pattern input = fromMaybe input $ Array.head $ String.split pattern input
        let noPrerelease = truncate (String.Pattern "-")
        let noBuild = truncate (String.Pattern "+")
        parseVersion Lenient (noPrerelease (noBuild string))
      Strict ->
        parseVersion Strict string

  either (_.error >>> StringParser.fail) pure parsed

toString :: forall f. Foldable f => f Char -> String
toString =
  CodeUnits.fromCharArray <<< Array.fromFoldable

chars :: Parser (List Char)
chars =
  StringParser.Combinators.many
    StringParser.CodeUnits.anyChar

charsUntilSpace :: Parser (List Char)
charsUntilSpace =
  StringParser.Combinators.manyTill
    StringParser.CodeUnits.anyChar
    (StringParser.CodeUnits.char ' ')

bumpPatch :: Version -> Version
bumpPatch (Version version) = Version (version { patch = version.patch + 1 })
