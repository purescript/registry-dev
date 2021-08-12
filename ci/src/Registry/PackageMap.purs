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

derive instance Functor PackageMap
derive instance Newtype (PackageMap a) _
derive newtype instance Foldable PackageMap

empty :: forall a. PackageMap a
empty = PackageMap Map.empty

singleton :: forall a. PackageName -> a -> PackageMap a
singleton k v = PackageMap $ Map.singleton k v

insert :: forall a. PackageName -> a -> PackageMap a -> PackageMap a
insert k v (PackageMap m) = PackageMap $ Map.insert k v m

insertWith :: forall a. (a -> a -> a) -> PackageName -> a -> PackageMap a -> PackageMap a
insertWith f k v (PackageMap m) = PackageMap $ Map.insertWith f k v m

union :: forall a. PackageMap a -> PackageMap a -> PackageMap a
union (PackageMap a) (PackageMap b) = PackageMap (Map.union a b)

lookup :: forall a. PackageName -> PackageMap a -> Maybe a
lookup k (PackageMap m) = Map.lookup k m

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
