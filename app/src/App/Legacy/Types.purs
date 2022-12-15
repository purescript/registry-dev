module Registry.App.Legacy.Types where

import Registry.App.Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Profunctor as Profunctor
import Registry.Internal.Codec as Internal.Codec
import Registry.PackageName as PackageName

-- | The format of a legacy packages.json package set file
newtype LegacyPackageSet = LegacyPackageSet (Map PackageName LegacyPackageSetEntry)

derive instance Newtype LegacyPackageSet _
derive newtype instance Eq LegacyPackageSet

legacyPackageSetCodec :: JsonCodec LegacyPackageSet
legacyPackageSetCodec =
  Profunctor.wrapIso LegacyPackageSet
    $ Internal.Codec.packageMap legacyPackageSetEntryCodec

-- | The union of all legacy package sets, with packages represented at all
-- | versions they have in the package sets.
type LegacyPackageSetUnion = Map PackageName (Map RawVersion (Map PackageName RawVersion))

legacyPackageSetUnionCodec :: JsonCodec LegacyPackageSetUnion
legacyPackageSetUnionCodec = Internal.Codec.packageMap $ rawVersionMapCodec $ Internal.Codec.packageMap rawVersionCodec

-- | The format of a legacy packages.json package set entry for an individual
-- | package.
type LegacyPackageSetEntry =
  { dependencies :: Array PackageName
  , repo :: String
  , version :: RawVersion
  }

legacyPackageSetEntryCodec :: JsonCodec LegacyPackageSetEntry
legacyPackageSetEntryCodec = CA.Record.object "LegacyPackageSetEntry"
  { dependencies: CA.array PackageName.codec
  , repo: CA.string
  , version: Profunctor.wrapIso RawVersion CA.string
  }

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
