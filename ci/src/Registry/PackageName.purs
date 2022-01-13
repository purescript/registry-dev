module Registry.PackageName
  ( PackageName
  , parse
  , print
  ) where

import Registry.Prelude

import Data.List as List
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Registry.Json (class RegistryJson)
import Registry.Json as Json
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodePoints as Parse
import Text.Parsing.StringParser.Combinators ((<?>))
import Text.Parsing.StringParser.Combinators as ParseC

newtype PackageName = PackageName String

derive newtype instance eqPackageName :: Eq PackageName
derive newtype instance ordPackageName :: Ord PackageName

instance RegistryJson PackageName where
  encode = Json.encode <<< print
  decode json = do
    package <- Json.decode json
    parse package # lmap (append "Expected PackageName: " <<< Parser.printParserError)

instance Show PackageName where
  show = print

print :: PackageName -> String
print (PackageName package) = package

parse :: String -> Either Parser.ParseError PackageName
parse = Parser.runParser do
  let
    -- Error messages which also define our rules for package names
    endErr = "Package name should end with a lower case char or digit"
    charErr = "Package name can contain lower case chars, digits and non-consecutive dashes"
    startErr = "Package name should start with a lower case char or a digit"
    manyDashesErr = "Package names cannot contain consecutive dashes"

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
    name = stripPureScriptPrefix $ fromCharArray $ List.toUnfoldable $ chars

  -- and check that it's not longer than 50 chars
  if String.length name > 50 then
    Parser.fail "Package name cannot be longer than 50 chars"
  else
    pure $ PackageName name
