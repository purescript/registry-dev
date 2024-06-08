-- | Implementation of the `PackageName` data type from the registry spec. A
-- | package name uniquely identifies a package.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#packagename
module Registry.PackageName
  ( PackageName
  , codec
  , parse
  , parser
  , print
  , stripPureScriptPrefix
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Alt ((<|>))
import Control.Monad.Except (Except, except)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Either (Either)
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe as Maybe
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Data.Tuple (fst)
import JSON (JSON)
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic

-- | Strip the "purescript-" prefix from a package name, if present.
-- |
-- | ```purs
-- | > stripPureScriptPrefix "purescript-numbers"
-- | "numbers"
-- |
-- | > stripPureScriptPrefix "numbers"
-- | "numbers"
-- | ```
stripPureScriptPrefix :: String -> String
stripPureScriptPrefix pkg =
  Maybe.fromMaybe pkg $ String.stripPrefix (String.Pattern "purescript-") pkg

-- | A Registry-compliant package name
newtype PackageName = PackageName String

derive newtype instance Eq PackageName
derive newtype instance Ord PackageName

-- | A codec for encoding and decoding a `PackageName` as a JSON string
codec :: CJ.Codec PackageName
codec = CJ.named "PackageName" $ Codec.codec' decode encode
  where
  decode :: JSON -> Except CJ.DecodeError PackageName
  decode = except <<< lmap CJ.DecodeError.basic <<< parse <=< Codec.decode CJ.string

  encode :: PackageName -> JSON
  encode = print >>> CJ.encode CJ.string

-- | Print a package name as a string
print :: PackageName -> String
print (PackageName package) = package

-- | Parse a package name from a string, reporting errors on failure
parse :: String -> Either String PackageName
parse = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser parser

-- | A parser for package names according to the registry spec:
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#packagename
parser :: Parser String PackageName
parser = do
  Parsing.ParseState unparsed _ _ <- Parsing.getParserT

  let
    -- Error messages which also define our rules for package names
    endErr = "Package name should end with a lower case char or digit"
    charErr = "Package name can contain lower case chars, digits and non-consecutive dashes"
    startErr = "Package name should start with a lower case char or a digit"
    prefixErr = "Package names should not begin with 'purescript-'"
    manyDashesErr = "Package names cannot contain consecutive dashes"

  -- Packages are not allowed to begin with purescript- in general, as that
  -- represents the legacy naming scheme and is almost certainly an error.
  -- However, packages can be explicitly blessed so they can use the prefix.
  let
    allowedPrefixNames =
      [ "purescript-compiler-backend-utilities"
      ]
    isBlessedPackage = unparsed `Array.elem` allowedPrefixNames
    hasPureScriptPrefix = isJust $ String.stripPrefix (String.Pattern "purescript-") unparsed

  when (hasPureScriptPrefix && not isBlessedPackage) do
    Parsing.fail prefixErr

  let
    acceptedChars = Parsing.Combinators.choice [ Parsing.String.Basic.lower, Parsing.String.Basic.digit ] <|> Parsing.fail charErr
    chunk1 = Parsing.Combinators.Array.many1 acceptedChars

  -- A "chunk" is a lowercase alphanumeric word delimited by dashes
  firstChunk <- chunk1 <|> Parsing.fail startErr

  nextChunks <- do
    chunks <- flip Parsing.Combinators.Array.manyTill_ Parsing.String.eof do
      _ <- Parsing.String.char '-' <|> Parsing.fail charErr
      _ <- Parsing.Combinators.optionMaybe (Parsing.Combinators.lookAhead Parsing.String.anyChar) >>= case _ of
        Just v
          | v == '-' -> Parsing.fail manyDashesErr
          | otherwise -> pure unit
        Nothing -> Parsing.Combinators.lookAhead Parsing.String.eof *> Parsing.fail endErr
      map (NonEmptyArray.cons '-') chunk1 <|> Parsing.fail endErr
    pure (fst chunks)

  -- Make sure that we consume all the string in input
  Parsing.String.eof <|> Parsing.fail charErr

  let
    allChunks = Array.concatMap NonEmptyArray.toArray (Array.cons firstChunk nextChunks)
    name = String.CodeUnits.fromCharArray allChunks

  if String.null name then
    Parsing.fail "Package name cannot be empty"
  else if String.length name > 150 then
    Parsing.fail "Package name cannot be longer than 150 characters"
  else
    pure $ PackageName name
