module Registry.App.Legacy.Types where

import Registry.App.Prelude

import Data.Codec.JSON as CJ
import Data.Profunctor as Profunctor
import Registry.Internal.Codec as Internal.Codec

-- | An unprocessed package name, which may possibly be malformed.
newtype RawPackageName = RawPackageName String

derive instance Newtype RawPackageName _
derive newtype instance Eq RawPackageName
derive newtype instance Ord RawPackageName

rawPackageNameCodec :: CJ.Codec RawPackageName
rawPackageNameCodec = Profunctor.wrapIso RawPackageName CJ.string

rawPackageNameMapCodec :: forall a. CJ.Codec a -> CJ.Codec (Map RawPackageName a)
rawPackageNameMapCodec = Internal.Codec.strMap "RawPackageMap" (Right <<< RawPackageName) (un RawPackageName)

-- | An unprocessed version
newtype RawVersion = RawVersion String

derive instance Newtype RawVersion _
derive newtype instance Eq RawVersion
derive newtype instance Ord RawVersion
