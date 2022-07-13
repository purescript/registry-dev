module Registry.Legacy.PackageSet where

import Registry.Prelude

import Registry.Json as Json
import Registry.PackageName (PackageName)

newtype LegacyPackageSet = LegacyPackageSet (Map PackageName LegacyPackageSetEntry)

derive instance Newtype LegacyPackageSet _
derive newtype instance Eq LegacyPackageSet
derive newtype instance Show LegacyPackageSet

instance RegistryJson LegacyPackageSet where
  encode (LegacyPackageSet plan) = Json.encode plan
  decode = map LegacyPackageSet <<< Json.decode

newtype LegacyPackageSetEntry = LegacyPackageSetEntry
  { dependencies :: Array PackageName
  , repo :: String
  , version :: RawVersion
  }

derive instance Newtype LegacyPackageSetEntry _
derive newtype instance Eq LegacyPackageSetEntry
derive newtype instance Show LegacyPackageSetEntry

instance RegistryJson LegacyPackageSetEntry where
  encode (LegacyPackageSetEntry plan) = Json.encode plan
  decode = map LegacyPackageSetEntry <<< Json.decode
