-- | Implementation of the `PackageSet` data type as described in the registry
-- | spec. A package set records a set of packages, each at a specific version,
-- | known to build together with the specified compiler version. Package sets
-- | contain other metadata, such as a package set version and a published time.
-- | Package sets are usually referred to by their version.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#35-package-set
-- |
-- | Package sets are published in the registry repository:
-- | https://github.com/purescript/registry/blob/main/package-sets
module Registry.PackageSet
  ( PackageSet(..)
  , codec
  ) where

import Prelude

import Data.Codec.JSON as CJ
import Data.DateTime (Date)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Profunctor as Profunctor
import Registry.Internal.Codec as Internal.Codec
import Registry.PackageName (PackageName)
import Registry.Version (Version)
import Registry.Version as Version

-- | A Registry package set, which contains a set of packages at specific
-- | versions known to compile together with the given compiler version.
newtype PackageSet = PackageSet
  { version :: Version
  , compiler :: Version
  , published :: Date
  , packages :: Map PackageName Version
  }

derive instance Newtype PackageSet _
derive newtype instance Eq PackageSet

-- | A codec for encoding and decoding a `PackageSet` as JSON. Represented as a
-- | JSON object. We use an explicit ordering instead of record sugar in the
-- | implementation.
codec :: CJ.Codec PackageSet
codec = Profunctor.wrapIso PackageSet $ CJ.named "PackageSet" $ CJ.object
  $ CJ.recordProp @"version" Version.codec
  $ CJ.recordProp @"compiler" Version.codec
  $ CJ.recordProp @"published" Internal.Codec.iso8601Date
  $ CJ.recordProp @"packages" (Internal.Codec.packageMap Version.codec)
  $ CJ.record
