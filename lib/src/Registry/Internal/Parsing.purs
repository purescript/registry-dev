-- | An internal module exporting helpers for the `parsing` library.
module Registry.Internal.Parsing where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple as Tuple
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String

chars :: Parser String (Array Char)
chars = Array.many Parsing.String.anyChar

charsUntil :: forall a. Parser String a -> Parser String (Array Char)
charsUntil = map Tuple.fst <<< Parsing.Combinators.Array.manyTill_ Parsing.String.anyChar

charsUntilSpace :: Parser String (Array Char)
charsUntilSpace = charsUntil (Parsing.String.char ' ')

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
    1 -> pure $ milliseconds' <> "0"
    2 -> pure $ milliseconds' <> "00"
    3 -> pure milliseconds'
    n -> Parsing.fail $ "Expected milliseconds with length 0-3, but received milliseconds with length: " <> show n
  pure
    { date: Array.fold [ year, "-", month, "-", day ]
    , time: Array.fold [ hour, ":", minute, ":", second ]
    , milliseconds
    }

