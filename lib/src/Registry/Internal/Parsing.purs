-- | An internal module exporting helpers for the `parsing` library.
module Registry.Internal.Parsing where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinatiors.Array
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic

-- | INTERNAL
-- |
-- | Parse as many BMP chars as possible
chars :: Parser String (Array Char)
chars = Array.many Parsing.String.anyChar

-- | INTERNAL
-- |
-- | Parse as many BMP chars as possible until some terminator. Consumes the
-- | terminator and returns the chars parsed.
charsUntil :: forall a. Parser String a -> Parser String (Array Char)
charsUntil = map Tuple.fst <<< Parsing.Combinators.Array.manyTill_ Parsing.String.anyChar

-- | INTERNAL
-- |
-- | Parse as many BMP chars as possible until a space. Consumes the space and
-- | returns the chars parsed.
charsUntilSpace :: Parser String (Array Char)
charsUntilSpace = charsUntil (Parsing.String.char ' ')

-- | INTERNAL
-- |
-- | A lenient RFC3339 parser that only parses the structure of the string
-- | without verifying components like valid dates. Suitable for migrating date
-- | strings between formats, but unsuitable for verifying the string represents
-- | an acceptable date.
rfc3339 :: Parser String { date :: String, time :: String, milliseconds :: String }
rfc3339 = do
  year <- Parsing.String.takeN 4
  _ <- Parsing.String.char '-'
  month <- Parsing.String.takeN 2
  _ <- Parsing.String.char '-'
  day <- Parsing.String.takeN 2
  _ <- Parsing.String.char 'T'
  hour <- Parsing.String.takeN 2
  _ <- Parsing.String.char ':'
  minute <- Parsing.String.takeN 2
  _ <- Parsing.String.char ':'
  second <- Parsing.String.takeN 2
  milliseconds' <- Parsing.Combinators.optionMaybe (Parsing.String.char '.') >>= case _ of
    Nothing -> Parsing.String.char 'Z' *> pure "000"
    Just _ -> charsUntil (Parsing.String.char 'Z') <#> CodeUnits.fromCharArray
  milliseconds <- case String.length milliseconds' of
    0 -> pure "000"
    1 -> pure $ milliseconds' <> "00"
    2 -> pure $ milliseconds' <> "0"
    3 -> pure milliseconds'
    n -> Parsing.fail $ "Expected milliseconds with length 0-3, but received milliseconds with length: " <> show n
  pure
    { date: Array.fold [ year, "-", month, "-", day ]
    , time: Array.fold [ hour, ":", minute, ":", second ]
    , milliseconds
    }

-- | INTERNAL
-- |
-- | A parser for registry-compliant Git urls that only admit the git or http
-- | protocols. Essentially, this regex:
-- | `(git|http(s)?)(:(//)?)([\w\.@\:/\-~]+)(\.git)?(/)?`
gitUrl :: Parser String String
gitUrl = do
  Parsing.ParseState inputStr _ _ <- Parsing.getParserT
  _ <- Parsing.String.string "git" <|> Parsing.String.string "https" <|> Parsing.String.string "http"
  _ <- Parsing.String.string "://"
  Tuple _ wasEOF <- do
    let terminator = Parsing.String.string ".git" $> false <|> Parsing.String.eof $> true
    let matcher = Parsing.String.Basic.alphaNum <|> Parsing.String.Basic.oneOf [ '.', '/', '@', '_', '-', '~' ]
    Parsing.Combinatiors.Array.manyTill_ matcher terminator

  if wasEOF then
    pure inputStr
  else do
    _ <- Parsing.Combinators.optional (Parsing.Combinators.try (Parsing.String.char '/'))
    _ <- Parsing.String.eof
    pure inputStr
