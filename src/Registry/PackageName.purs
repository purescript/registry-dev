module Registry.PackageName
  ( PackageName
  , parse
  , print
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.String as String
import Data.String.CodeUnits as String.CodeUnits
import Parsing (ParseError)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic
import Registry.Json (class StringEncodable)
import Registry.Json as Json

newtype PackageName = PackageName String

derive newtype instance Eq PackageName
derive newtype instance Ord PackageName

instance Show PackageName where
  show = print

instance StringEncodable PackageName where
  toEncodableString = print
  fromEncodableString = lmap (append "Expected PackageName: " <<< Parsing.parseErrorMessage) <<< parse

instance RegistryJson PackageName where
  encode = Json.encode <<< Json.toEncodableString
  decode = Json.fromEncodableString <=< Json.decode

print :: PackageName -> String
print (PackageName package) = package

parse :: String -> Either ParseError PackageName
parse inputStr = Parsing.runParser inputStr do
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
    isBlessedPackage = inputStr `Array.elem` allowedPrefixNames
    hasPureScriptPrefix = isJust $ String.stripPrefix (String.Pattern "purescript-") inputStr

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

  -- and check that it's not longer than 50 chars
  if String.length name > 50 then
    Parsing.fail "Package name cannot be longer than 50 chars"
  else
    pure $ PackageName name
