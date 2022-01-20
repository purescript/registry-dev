module Registry.Version
  ( Version
  , major
  , minor
  , patch
  , rawVersion
  , printVersion
  , parseVersion
  , Range
  , greaterThanOrEq
  , lessThan
  , printRange
  , rawRange
  , ParseMode(..)
  , parseRange
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.Function (on)
import Data.Int as Int
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Foreign.SemVer as SemVer
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

instance RegistryJson Version where
  encode = Json.encode <<< printVersion
  decode json = do
    string <- Json.decode json
    lmap StringParser.printParserError $ parseVersion Strict string

instance Show Version where
  show = printVersion

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
    strictParser :: Parser Range
    strictParser = do
      _ <- StringParser.CodeUnits.string ">="
      lhs <- toVersion =<< map toString charsUntilSpace
      _ <- StringParser.CodeUnits.char '<'
      rhs <- toVersion =<< map toString chars
      StringParser.CodeUnits.eof
      when (lhs >= rhs) do
        StringParser.fail $ Array.fold
          [ "Left-hand version ("
          , printVersion lhs
          , ") must be less than right-hand version ("
          , printVersion rhs
          , ")"
          ]
      pure $ Range { lhs, rhs, mode, raw: input }

  case mode of
    Lenient -> case SemVer.parseRange input of
      Nothing ->
        Left { pos: 0, error: "Unable to parse SemVer range in lenient mode." }
      Just parsed -> do
        let trimPrereleaseGuards = String.replaceAll (String.Pattern "-0") (String.Replacement "")
        StringParser.runParser strictParser $ trimPrereleaseGuards parsed
    Strict ->
      StringParser.runParser strictParser input
  where
  toVersion string =
    case parseVersion mode string of
      Left { error } ->
        StringParser.fail error
      Right parsed ->
        pure parsed

  toString =
    CodeUnits.fromCharArray <<< Array.fromFoldable

  chars =
    StringParser.Combinators.many
      StringParser.CodeUnits.anyChar

  charsUntilSpace =
    StringParser.Combinators.manyTill
      StringParser.CodeUnits.anyChar
      (StringParser.CodeUnits.char ' ')

data ParseMode = Lenient | Strict

derive instance Eq ParseMode
