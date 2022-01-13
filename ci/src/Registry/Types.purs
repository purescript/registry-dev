module Registry.Types where

import Registry.Prelude

import Registry.Json (class RegistryJson)

-- | An unprocessed package name, which may possibly be malformed.
newtype RawPackageName = RawPackageName String

derive instance Newtype RawPackageName _
derive newtype instance Eq RawPackageName
derive newtype instance Ord RawPackageName
derive newtype instance Show RawPackageName
derive newtype instance RegistryJson RawPackageName

-- | An unprocessed version, taken from a GitHub tag
newtype RawVersion = RawVersion String

derive instance Newtype RawVersion _
derive newtype instance Eq RawVersion
derive newtype instance Ord RawVersion
derive newtype instance Show RawVersion
derive newtype instance RegistryJson RawVersion
