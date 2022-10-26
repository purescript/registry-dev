module Registry.Types where

import Prelude

import Data.Newtype (class Newtype)
import Registry.Json (class RegistryJson, class StringEncodable)

-- | An unprocessed package name, which may possibly be malformed.
newtype RawPackageName = RawPackageName String

derive instance Newtype RawPackageName _
derive newtype instance Eq RawPackageName
derive newtype instance Ord RawPackageName
derive newtype instance StringEncodable RawPackageName
derive newtype instance RegistryJson RawPackageName

-- | An unprocessed version
newtype RawVersion = RawVersion String

derive instance Newtype RawVersion _
derive newtype instance Eq RawVersion
derive newtype instance Ord RawVersion
derive newtype instance StringEncodable RawVersion
derive newtype instance RegistryJson RawVersion

-- | An unprocessed version range
newtype RawVersionRange = RawVersionRange String

derive instance Newtype RawVersionRange _
derive newtype instance Eq RawVersionRange
derive newtype instance Ord RawVersionRange
derive newtype instance StringEncodable RawVersionRange
derive newtype instance RegistryJson RawVersionRange
