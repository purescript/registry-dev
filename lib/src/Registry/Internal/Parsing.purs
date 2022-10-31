-- | An internal module exporting helpers for the `parsing` library.
module Registry.Internal.Parsing where

import Prelude

import Data.Array as Array
import Data.Tuple as Tuple
import Parsing (Parser)
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String

chars :: Parser String (Array Char)
chars = Array.many Parsing.String.anyChar

charsUntil :: forall a. Parser String a -> Parser String (Array Char)
charsUntil = map Tuple.fst <<< Parsing.Combinators.Array.manyTill_ Parsing.String.anyChar

charsUntilSpace :: Parser String (Array Char)
charsUntilSpace = charsUntil (Parsing.String.char ' ')
