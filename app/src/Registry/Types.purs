module Registry.Types where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Profunctor as Profunctor
import Registry.Internal.Codec as Internal.Codec

-- | An unprocessed package name, which may possibly be malformed.
newtype RawPackageName = RawPackageName String

derive instance Newtype RawPackageName _
derive newtype instance Eq RawPackageName
derive newtype instance Ord RawPackageName

rawPackageNameCodec :: JsonCodec RawPackageName
rawPackageNameCodec = Profunctor.wrapIso RawPackageName CA.string

rawPackageNameMapCodec :: forall a. JsonCodec a -> JsonCodec (Map RawPackageName a)
rawPackageNameMapCodec = Internal.Codec.strMap "RawPackageMap" (Just <<< RawPackageName) (un RawPackageName)

-- | An unprocessed version
newtype RawVersion = RawVersion String

derive instance Newtype RawVersion _
derive newtype instance Eq RawVersion
derive newtype instance Ord RawVersion

rawVersionCodec :: JsonCodec RawVersion
rawVersionCodec = Profunctor.wrapIso RawVersion CA.string

rawVersionMapCodec :: forall a. JsonCodec a -> JsonCodec (Map RawVersion a)
rawVersionMapCodec = Internal.Codec.strMap "RawVersionMap" (Just <<< RawVersion) (un RawVersion)

-- | An unprocessed version range
newtype RawVersionRange = RawVersionRange String

derive instance Newtype RawVersionRange _
derive newtype instance Eq RawVersionRange
derive newtype instance Ord RawVersionRange

rawVersionRangeCodec :: JsonCodec RawVersionRange
rawVersionRangeCodec = Profunctor.wrapIso RawVersionRange CA.string
