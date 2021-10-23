module Registry.Error where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))

mkError :: forall e a. e -> Either (NonEmptyArray e) a
mkError = Left <<< NEA.singleton