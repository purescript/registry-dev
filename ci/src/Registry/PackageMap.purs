module Registry.PackageMap where

import Registry.Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError, decodeJson, encodeJson, fromString)
import Data.Bitraversable (bitraverse)
import Data.Foldable (class Foldable)
import Data.Map as Map
import Foreign.Object as Object
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName

-- | A map using package names as its keys. Implemented as as a newtype so as to
-- | encode and decode package maps as objects with string keys, and not as
-- | arrays of tuples.
newtype PackageMap a = PackageMap (Map PackageName a)

derive instance Newtype (PackageMap a) _
derive newtype instance Foldable PackageMap

instance EncodeJson a => EncodeJson (PackageMap a) where
  encodeJson (PackageMap m) = do
    let
      pairs :: Array (Tuple PackageName a)
      pairs = Map.toUnfoldable m

    encodeJson $ Object.fromFoldable $ map (lmap PackageName.print) pairs

instance DecodeJson a => DecodeJson (PackageMap a) where
  decodeJson json = do
    obj <- decodeJson json

    let
      pairs :: Array (Tuple String Json)
      pairs = Object.toUnfoldable obj

      decodePair :: Tuple String Json -> Either JsonDecodeError (Tuple PackageName a)
      decodePair = bitraverse (decodeJson <<< fromString) decodeJson

    packages <- traverse decodePair pairs
    pure $ PackageMap $ Map.fromFoldable packages
