-- | Implementation of the `Version` data type from the registry spec. A version
-- | represents the source code of a package at a particular point in time.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#version
module Registry.Version
  ( Version
  , bumpHighest
  , bumpMajor
  , bumpMinor
  , bumpPatch
  , codec
  , major
  , minor
  , parse
  , parser
  , patch
  , print
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Except (Except, except)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Either (Either)
import Data.Int as Int
import Data.Maybe (maybe)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import JSON (JSON)
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic

-- | A Registry-compliant version of the form 'X.Y.Z', where each place is a
-- | non-negative integer.
newtype Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  }

instance Eq Version where
  eq (Version l) (Version r) =
    case l.major == r.major of
      true ->
        case l.minor == r.minor of
          true -> l.patch == r.patch
          x -> x
      x -> x

instance Ord Version where
  compare (Version l) (Version r) =
    case l.major `compare` r.major of
      EQ ->
        case l.minor `compare` r.minor of
          EQ -> l.patch `compare` r.patch
          x -> x
      x -> x

-- | A codec for encoding and decoding a `Version` as a JSON string.
codec :: CJ.Codec Version
codec = CJ.named "Version" $ Codec.codec' decode encode
  where
  decode :: JSON -> Except CJ.DecodeError Version
  decode = except <<< lmap CJ.DecodeError.basic <<< parse <=< Codec.decode CJ.string

  encode :: Version -> JSON
  encode = print >>> CJ.encode CJ.string

-- | Print a `Version` as a string of the form "X.Y.Z"
print :: Version -> String
print (Version v) = do
  let places = map (Int.toStringAs Int.decimal) [ v.major, v.minor, v.patch ]
  String.joinWith "." places

-- | Parse a `Version` from a string, failing if the input string does not
-- | represent a Registry-compliant version of the form "X.Y.Z" where X, Y,
-- | and Z are non-negative integers.
parse :: String -> Either String Version
parse = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser parser

-- | A strict parser for Registry-compliant versions. This parser will fail on
-- | leading or trailing characters (such as spaces or 'v').
parser :: Parser String Version
parser = do
  major' <- nonNegativeInt
  _ <- Parsing.String.char '.'
  minor' <- nonNegativeInt
  _ <- Parsing.String.char '.'
  patch' <- nonNegativeInt
  Parsing.String.eof
  pure $ Version { major: major', minor: minor', patch: patch' }
  where
  nonNegativeInt :: Parser String Int
  nonNegativeInt = do
    digitChars <- Parsing.Combinators.Array.many1 Parsing.String.Basic.digit
    let
      zeroCount = Array.length $ NonEmptyArray.takeWhile (_ == '0') digitChars
      digitString = CodeUnits.fromCharArray $ NonEmptyArray.toArray digitChars
      failInteger = Parsing.fail $ "Invalid 32-bit integer: " <> digitString
    integer <- maybe failInteger pure $ Int.fromString digitString
    -- We do not accept leading zeros in versions
    when (zeroCount > 1 || (zeroCount == 1 && integer /= 0)) do
      Parsing.fail $ "Leading zeros are not allowed: " <> digitString
    when (integer < 0) do
      Parsing.fail $ "Invalid non-negative integer: " <> show integer
    pure integer

-- | Retrieve the major place of the version
major :: Version -> Int
major (Version version) = version.major

-- | Retrieve the minor place of the version
minor :: Version -> Int
minor (Version version) = version.minor

-- | Retrieve the patch place of the version
patch :: Version -> Int
patch (Version version) = version.patch

-- | Increments the greatest position.
-- |   - 0.0.1 becomes 0.0.2
-- |   - 0.1.1 becomes 0.2.0
-- |   - 1.1.1 becomes 2.0.0
bumpHighest :: Version -> Version
bumpHighest version@(Version v) =
  if v.major >= 1 then bumpMajor version
  else if v.minor >= 1 then bumpMinor version
  else bumpPatch version

-- | Increments the 'major' place of the version, setting all other places to 0.
-- |   - 1.0.0 becomes 2.0.0
-- |   - 1.2.3 becomes 2.0.0
bumpMajor :: Version -> Version
bumpMajor (Version version) = Version (version { major = version.major + 1, minor = 0, patch = 0 })

-- | Increments the 'major' place of the version, setting the patch place to 0.
-- |   - 1.0.0 becomes 1.1.0
-- |   - 1.2.3 becomes 1.3.0
bumpMinor :: Version -> Version
bumpMinor (Version version) = Version (version { minor = version.minor + 1, patch = 0 })

-- | Increments the 'patch' place of the version, leaving other places untouched.
-- |   - 1.0.0 becomes 1.0.1
-- |   - 1.2.3 becomes 1.2.4
bumpPatch :: Version -> Version
bumpPatch (Version version) = Version (version { patch = version.patch + 1 })
