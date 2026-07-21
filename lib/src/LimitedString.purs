-- | A string whose maximum length is tracked at the type level.
module Registry.LimitedString
  ( LimitedString
  , codec
  , parse
  , print
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Control.Monad.Except (Except, except)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Reflectable (class Reflectable, reflectType)
import Data.String as String
import JSON (JSON)
import Type.Proxy (Proxy(..))

-- | A string that cannot exceed the given number of characters.
newtype LimitedString (limit :: Int) = LimitedString String

derive newtype instance Eq (LimitedString limit)
derive newtype instance Ord (LimitedString limit)

-- | A codec for encoding and decoding a limited string as a JSON string.
codec :: forall limit. Reflectable limit Int => CJ.Codec (LimitedString limit)
codec = CJ.named "LimitedString" $ Codec.codec' decode encode
  where
  decode :: JSON -> Except CJ.DecodeError (LimitedString limit)
  decode = except <<< lmap CJ.DecodeError.basic <<< parse <=< Codec.decode CJ.string

  encode :: LimitedString limit -> JSON
  encode = print >>> CJ.encode CJ.string

-- | Parse a string, rejecting values that exceed the type-level limit.
parse :: forall limit. Reflectable limit Int => String -> Either String (LimitedString limit)
parse string = do
  let limit = reflectType (Proxy @limit)
  if String.length string > limit then
    Left $ "LimitedString: Exceeds limit of " <> show limit <> " characters."
  else
    Right $ LimitedString string

-- | Print a limited string as a plain string.
print :: forall limit. LimitedString limit -> String
print (LimitedString string) = string
