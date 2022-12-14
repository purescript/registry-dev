module Registry.App.Legacy.LenientRange
  ( LenientRange(..)
  , parse
  , print
  , range
  , raw
  ) where

import Registry.App.Prelude

import Control.Monad.Error.Class as Error
import Data.Array as Array
import Data.CodePoint.Unicode as Unicode
import Data.Either (fromRight)
import Data.Function (on)
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.String as Parsing.String
import Registry.App.Legacy.LenientVersion (LenientVersion(..))
import Registry.App.Legacy.LenientVersion as LenientVersion
import Registry.Foreign.SemVer as SemVer
import Registry.Internal.Parsing as Internal.Parsing
import Registry.Range as Range
import Registry.Version as Version

-- | A lenient variant on the `Registry.Range` type, which attempts to correct
-- | issues with the input string while parsing and retains the raw input.
newtype LenientRange = LenientRange { range :: Range, raw :: String }

derive instance Newtype LenientRange _

instance Eq LenientRange where
  eq = eq `on` (_.range <<< un LenientRange)

-- | Print the `Range` from a lenient range in the form ">=X.Y.Z <X.Y.Z"
print :: LenientRange -> String
print (LenientRange r) = Range.print r.range

-- | Extract the `Range` from a lenient range
range :: LenientRange -> Range
range (LenientRange r) = r.range

-- | Print the raw string used to produce a lenient range.
raw :: LenientRange -> String
raw (LenientRange r) = r.raw

-- | Parse a `Range` from a string, fixing issues with the input string where
-- | possible, such as trimming prerelease identifiers, and trimming leading and
-- | trailing spaces.
parse :: String -> Either String LenientRange
parse input = lmap Parsing.parseErrorMessage do
  -- We trim off prerelease identifiers and build metadata in lenient parsing
  -- mode. Notably, we do not do this at the version level -- even lenient
  -- version parsing will fail if these identifiers are present -- so we need
  -- to pass through the range and do the work here.
  --
  -- Trimming prerelease identifiers in lenient mode can produce ranges
  -- where the lhs was less than the rhs, but no longer is. For example:
  -- '>=1.0.0-rc.1 <1.0.0-rc.5' -> '>=1.0.0 <1.0.0'.
  --
  -- We fix these ranges by bumping the rhs patch by one.
  { lhs, rhs } <- Parsing.runParser (convertRange input) do
    _ <- Parsing.String.string ">=" <|> Parsing.fail "Ranges must begin with >="
    LenientVersion { version: lhs } <- versionParser =<< map String.CodeUnits.fromCharArray Internal.Parsing.charsUntilSpace
    _ <- Parsing.String.char '<' <|> Parsing.fail "Ranges must end with <"
    LenientVersion { version: rhs } <- versionParser =<< map String.CodeUnits.fromCharArray Internal.Parsing.chars
    let rhs' = if lhs == rhs then Version.bumpPatch rhs else rhs
    pure { lhs, rhs: rhs' }

  -- If we were able to parse out an lhs and rhs, then we can attempt to parse
  -- a well-formed range using them.
  let normalizedRange = Array.fold [ ">=", Version.print lhs, " <", Version.print rhs ]
  parsed <- Parsing.runParser normalizedRange Range.parser
  pure $ LenientRange { raw: input, range: parsed }

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
  semVer <- note "Could not parse with parseRange" (SemVer.parseRange input)
  pure $ semVer
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
    lhsChars <- map String.CodeUnits.fromCharArray Internal.Parsing.charsUntilSpace
    LenientVersion { version: lhs } <- versionParser lhsChars
    Parsing.ParseState suffix _ _ <- Parsing.getParserT
    pure $ Array.fold [ ">=", Version.print (Version.bumpPatch lhs), " ", suffix ]

  -- Fix ranges that end with '<=' instead of '<'
  fixLtRhs str = fromRight str $ Parsing.runParser str do
    _ <- Parsing.String.string ">="
    LenientVersion { version: lhs } <- versionParser =<< map String.CodeUnits.fromCharArray Internal.Parsing.charsUntilSpace
    _ <- Parsing.String.string "<="
    LenientVersion { version: rhs } <- versionParser =<< map String.CodeUnits.fromCharArray Internal.Parsing.chars
    Parsing.String.eof
    pure $ Array.fold [ ">=", Version.print lhs, " <", Version.print (Version.bumpPatch rhs) ]

  -- Fix ranges that only consist of an upper bound ('<1.0.0')
  fixUpperOnly str = fromRight str $ Parsing.runParser str do
    _ <- Parsing.String.char '<'
    hasEq <- Parsing.Combinators.optionMaybe (Parsing.String.char '=')
    LenientVersion { version: lhs } <- versionParser =<< map String.CodeUnits.fromCharArray Internal.Parsing.chars
    Parsing.String.eof
    pure $ Array.fold [ ">=0.0.0 <", Version.print (if isJust hasEq then Version.bumpPatch lhs else lhs) ]

  -- Replace exact ranges ('1.0.0') with ranges ('>=1.0.0 <1.0.1')
  fixExactVersions str = fromRight str do
    LenientVersion { version } <- LenientVersion.parse str
    pure $ Array.fold [ ">=", Version.print version, " <", Version.print (Version.bumpPatch version) ]

versionParser :: String -> Parser String LenientVersion
versionParser =
  Error.liftEither
    <<< flip Parsing.runParser LenientVersion.parser
    <<< String.takeWhile (Unicode.isAlphaNum || eq (String.codePointFromChar '.'))
