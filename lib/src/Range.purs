-- | Implementation of the `Range` data type from the registry spec. Packages
-- | specify their dependencies using version ranges.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#range
module Registry.Range
  ( Range
  , codec
  , greaterThanOrEq
  , includes
  , intersect
  , lessThan
  , parse
  , parser
  , print
  , union
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class as Error
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either, hush)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String.CodeUnits
import Parsing (Parser)
import Parsing as Parsing
import Parsing.String as Parsing.String
import Registry.Internal.Parsing as Internal.Parsing
import Registry.Version (Version)
import Registry.Version as Version

-- | A Registry-compliant version range of the form '>=X.Y.Z <X.Y.Z', where the
-- | left-hand version is less than the right-hand version.
newtype Range = Range
  { lhs :: Version
  , rhs :: Version
  }

instance Eq Range where
  eq = eq `on` (\(Range { lhs, rhs }) -> [ lhs, rhs ])

-- | A codec for encoding and decoding a `Range` as JSON. Ranges are encoded as
-- | strings of the form ">=X.Y.Z <X.Y.Z".
codec :: JsonCodec Range
codec = CA.prismaticCodec "Range" (hush <<< parse) print CA.string

-- | Print a range in the form ">=X.Y.Z <X.Y.Z"
print :: Range -> String
print (Range { lhs, rhs }) =
  Array.fold
    [ ">="
    , Version.print lhs
    , " <"
    , Version.print rhs
    ]

-- | Parse a string representing a range, ie. a string which must be of the form
-- | ">=X.Y.Z <X.Y.Z" where "X.Y.Z" is a valid `Version`.
parse :: String -> Either String Range
parse = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser parser

-- | A parser for strings that represent a `Range`. This parser will fail on
-- | leading or trailing spaces.
parser :: Parser String Range
parser = do
  _ <- Parsing.String.string ">=" <|> Parsing.fail "Ranges must begin with >="
  lhs <- parseVersion =<< map String.CodeUnits.fromCharArray Internal.Parsing.charsUntilSpace
  _ <- Parsing.String.char '<' <|> Parsing.fail "Ranges must end with <"
  rhs <- parseVersion =<< map String.CodeUnits.fromCharArray Internal.Parsing.chars
  Parsing.String.eof
  if (lhs >= rhs) then
    Parsing.fail $ Array.fold
      [ "Left-hand version ("
      , Version.print lhs
      , ") must be less than right-hand version ("
      , Version.print rhs
      , ")"
      ]
  else pure $ Range { lhs, rhs }
  where
  -- We want to parse versions as strings with no trailing characters, without
  -- affecting the overall parse position.
  parseVersion :: String -> Parser String Version
  parseVersion input = Error.liftEither (Parsing.runParser input Version.parser)

-- | Retrieve the left-hand side of a range, ie. the 'greater than or eq' part
-- | of the range.
-- |
-- | Given the range ">=1.0.0 <2.0.0", produces the version "1.0.0".
greaterThanOrEq :: Range -> Version
greaterThanOrEq (Range range) = range.lhs

-- | Retrieve the right-hand side of a range, ie. the 'less than' part of the
-- | range.
-- |
-- | Given the range ">=1.0.0 <2.0.0", produces the version "2.0.0".
lessThan :: Range -> Version
lessThan (Range range) = range.rhs

-- | Check whether a range includes the provided version.
includes :: Range -> Version -> Boolean
includes (Range { lhs, rhs }) version = version >= lhs && version < rhs

-- | Union two ranges, taking the minimum of the left-hand sides and the maximum
-- | of the right-hand sides.
-- |
-- | Given the ranges ">=1.0.0 <3.0.0" ">=2.0.0 <4.0.0", a union will produce
-- | ">=1.0.0 <4.0.0".
union :: Range -> Range -> Range
union (Range r1) (Range r2) = do
  let lhs = min r1.lhs r2.lhs
  let rhs = max r1.rhs r2.rhs
  Range { lhs, rhs }

-- | Intersect two ranges, taking the maximum of the left-hand sides and the
-- | minimum of the right-hand sides.
-- |
-- | Given the ranges ">=1.0.0 <3.0.0" ">=2.0.0 <4.0.0", an intersection will
-- | produce ">=2.0.0 <3.0.0".
intersect :: Range -> Range -> Maybe Range
intersect (Range r1) (Range r2)
  | r1.lhs >= r2.rhs || r2.lhs >= r1.rhs = Nothing
  | otherwise = Just $ Range
      { lhs: max r1.lhs r2.lhs
      , rhs: min r1.rhs r2.rhs
      }
