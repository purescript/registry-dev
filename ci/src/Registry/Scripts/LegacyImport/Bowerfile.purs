module Registry.Scripts.LegacyImport.Bowerfile
  ( Bowerfile(..)
  , toManifestFields
  , bowerfileCodec
  ) where

import Registry.Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Profunctor (dimap)
import Registry.Scripts.LegacyImport.ManifestFields (ManifestFields, manifestFieldsCodec)

toManifestFields :: Bowerfile -> ManifestFields
toManifestFields (Bowerfile fields) = fields

newtype Bowerfile = Bowerfile ManifestFields

derive newtype instance Eq Bowerfile
derive newtype instance Show Bowerfile

bowerfileCodec :: JsonCodec Bowerfile
bowerfileCodec = dimap (\(Bowerfile a) -> a) Bowerfile manifestFieldsCodec
