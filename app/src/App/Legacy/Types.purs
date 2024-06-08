module Registry.App.Legacy.Types where

import Registry.App.Prelude

import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Profunctor as Profunctor
import Registry.Internal.Codec as Internal.Codec
import Registry.PackageName as PackageName

-- | The format of a legacy packages.json package set file
newtype LegacyPackageSet = LegacyPackageSet (Map PackageName LegacyPackageSetEntry)

derive instance Newtype LegacyPackageSet _
derive newtype instance Eq LegacyPackageSet

legacyPackageSetCodec :: CJ.Codec LegacyPackageSet
legacyPackageSetCodec =
  Profunctor.wrapIso LegacyPackageSet
    $ Internal.Codec.packageMap legacyPackageSetEntryCodec

-- | The union of all legacy package sets, with packages represented at all
-- | versions they have in the package sets.
type LegacyPackageSetUnion = Map PackageName (Map RawVersion (Map PackageName { min :: RawVersion, max :: RawVersion }))

legacyPackageSetUnionCodec :: CJ.Codec LegacyPackageSetUnion
legacyPackageSetUnionCodec = Internal.Codec.packageMap $ rawVersionMapCodec $ Internal.Codec.packageMap
  $ CJ.named "LenientBounds"
  $ CJ.Record.object
      { min: rawVersionCodec
      , max: rawVersionCodec
      }

-- | The format of a legacy packages.json package set entry for an individual
-- | package.
type LegacyPackageSetEntry =
  { dependencies :: Array PackageName
  , repo :: String
  , version :: RawVersion
  }

legacyPackageSetEntryCodec :: CJ.Codec LegacyPackageSetEntry
legacyPackageSetEntryCodec = CJ.named "LegacyPackageSetEntry" $ CJ.Record.object
  { dependencies: CJ.array PackageName.codec
  , repo: CJ.string
  , version: Profunctor.wrapIso RawVersion CJ.string
  }

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

rawVersionCodec :: CJ.Codec RawVersion
rawVersionCodec = Profunctor.wrapIso RawVersion CJ.string

rawVersionMapCodec :: forall a. CJ.Codec a -> CJ.Codec (Map RawVersion a)
rawVersionMapCodec = Internal.Codec.strMap "RawVersionMap" (Right <<< RawVersion) (un RawVersion)

-- | An unprocessed version range
newtype RawVersionRange = RawVersionRange String

derive instance Newtype RawVersionRange _
derive newtype instance Eq RawVersionRange
derive newtype instance Ord RawVersionRange

rawVersionRangeCodec :: CJ.Codec RawVersionRange
rawVersionRangeCodec = Profunctor.wrapIso RawVersionRange CJ.string
