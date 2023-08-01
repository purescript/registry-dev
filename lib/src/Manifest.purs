-- | Implementation of the `Manifest` data type representing a `purs.json` file,
-- | as described in the registry spec. Every package in the registry contains
-- | a manifest file in the root of the package tarball. Manifests contain
-- | critical package information such as the package name, version, license,
-- | dependencies, and more.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#manifest
-- |
-- | There is also a Dhall spec for this data type:
-- | https://github.com/purescript/registry-dev/blob/master/specs/v1/Manifest.dhall
-- |
-- | Package manifests can be found in the tarball for each package, and they
-- | are also cached in the registry index:
-- | https://github.com/purescript/registry-index
module Registry.Manifest
  ( Manifest(..)
  , codec
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor as Profunctor
import Data.String.NonEmpty (NonEmptyString)
import Registry.Internal.Codec as Internal.Codec
import Registry.License (License)
import Registry.License as License
import Registry.Location (Location)
import Registry.Location as Location
import Registry.Owner (Owner)
import Registry.Owner as Owner
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Version (Version)
import Registry.Version as Version
import Type.Proxy (Proxy(..))

-- | The manifest for a package version, which records critical information for
-- | the registry, pursuit, and package managers to use.
newtype Manifest = Manifest
  { name :: PackageName
  , version :: Version
  , license :: License
  , location :: Location
  , owners :: Maybe (NonEmptyArray Owner)
  , description :: Maybe String
  , includeFiles :: Maybe (NonEmptyArray NonEmptyString)
  , excludeFiles :: Maybe (NonEmptyArray NonEmptyString)
  , dependencies :: Map PackageName Range
  }

derive instance Newtype Manifest _
derive instance Eq Manifest

-- There cannot be two manifests that share a name and version, so we can order
-- manifests by these two fields. This instance is useful when constructing sets
-- or de-duplicating arrays with `nub`.
instance Ord Manifest where
  compare (Manifest a) (Manifest b) = Array.fold
    [ compare a.name b.name
    , compare a.version b.version
    ]

-- | A codec for encoding and decoding a `Manifest` as JSON. Represented as a
-- | JSON object. The implementation uses explicitly ordered keys instead of
-- | record sugar.
codec :: JsonCodec Manifest
codec = Profunctor.wrapIso Manifest $ CA.object "Manifest"
  $ CA.recordProp (Proxy :: _ "name") PackageName.codec
  $ CA.recordProp (Proxy :: _ "version") Version.codec
  $ CA.recordProp (Proxy :: _ "license") License.codec
  $ CA.recordPropOptional (Proxy :: _ "description") (Internal.Codec.limitedString 300)
  $ CA.recordProp (Proxy :: _ "location") Location.codec
  $ CA.recordPropOptional (Proxy :: _ "owners") (CA.Common.nonEmptyArray Owner.codec)
  $ CA.recordPropOptional (Proxy :: _ "includeFiles") (CA.Common.nonEmptyArray CA.Common.nonEmptyString)
  $ CA.recordPropOptional (Proxy :: _ "excludeFiles") (CA.Common.nonEmptyArray CA.Common.nonEmptyString)
  $ CA.recordProp (Proxy :: _ "dependencies") (Internal.Codec.packageMap Range.codec)
  $ CA.record
