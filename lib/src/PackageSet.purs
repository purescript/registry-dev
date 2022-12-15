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

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.DateTime (Date)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Profunctor as Profunctor
import Registry.Internal.Codec as Internal.Codec
import Registry.PackageName (PackageName)
import Registry.Version (Version)
import Registry.Version as Version
import Type.Proxy (Proxy(..))

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
codec :: JsonCodec PackageSet
codec = Profunctor.wrapIso PackageSet $ CA.object "PackageSet"
  $ CA.recordProp (Proxy :: _ "version") Version.codec
  $ CA.recordProp (Proxy :: _ "compiler") Version.codec
  $ CA.recordProp (Proxy :: _ "published") Internal.Codec.iso8601Date
  $ CA.recordProp (Proxy :: _ "packages") (Internal.Codec.packageMap Version.codec)
  $ CA.record
