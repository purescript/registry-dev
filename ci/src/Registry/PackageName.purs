module Registry.PackageName
  ( PackageName
  , parse
  , print
  ) where

import Registry.Prelude

import Data.Array as Array
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Registry.Json (class StringEncodable)
import Registry.Json as Json
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodePoints as Parse
import Text.Parsing.StringParser.Combinators ((<?>))
import Text.Parsing.StringParser.Combinators as ParseC

newtype PackageName = PackageName String

derive newtype instance Eq PackageName
derive newtype instance Ord PackageName

instance Show PackageName where
  show = print

instance StringEncodable PackageName where
  toEncodableString = print
  fromEncodableString = lmap (append "Expected PackageName: " <<< Parser.printParserError) <<< parse

instance RegistryJson PackageName where
  encode = Json.encode <<< Json.toEncodableString
  decode = Json.fromEncodableString <=< Json.decode

print :: PackageName -> String
print (PackageName package) = package

parse :: String -> Either Parser.ParseError PackageName
parse inputStr = flip Parser.runParser inputStr do
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
    Parser.fail prefixErr

  let
    char = ParseC.choice [ Parse.lowerCaseChar, Parse.anyDigit ] <?> charErr
    dash = void $ Parse.char '-'
    chunk = ParseC.many1 char

  -- A "chunk" is an alphanumeric word between dashes
  firstChunk <- chunk <?> startErr

  nextChunks <- do
    chunks <- ParseC.many do
      void dash
      void $ ParseC.optionMaybe (ParseC.lookAhead Parse.anyChar) >>= case _ of
        Just v
          | v == '-' -> Parser.fail manyDashesErr
          | otherwise -> pure unit
        Nothing -> ParseC.lookAhead Parse.eof *> Parser.fail endErr
      map (NEL.cons '-') chunk <?> endErr
    pure chunks

  -- Make sure that we consume all the string in input
  Parse.eof <?> charErr

  -- Put together the string, stripping the "purescript-" prefix if there
  let
    chars = List.concat $ map NEL.toList $ List.Cons firstChunk nextChunks
    name = fromCharArray $ List.toUnfoldable $ chars

  -- and check that it's not longer than 50 chars
  if String.length name > 50 then
    Parser.fail "Package name cannot be longer than 50 chars"
  else
    pure $ PackageName name
