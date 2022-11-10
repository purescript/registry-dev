module Registry.Types where

import Prelude

import Data.Newtype (class Newtype)

-- | An unprocessed package name, which may possibly be malformed.
newtype RawPackageName = RawPackageName String

derive instance Newtype RawPackageName _
derive newtype instance Eq RawPackageName
derive newtype instance Ord RawPackageName

-- | An unprocessed version
newtype RawVersion = RawVersion String

derive instance Newtype RawVersion _
derive newtype instance Eq RawVersion
derive newtype instance Ord RawVersion

-- | An unprocessed version range
newtype RawVersionRange = RawVersionRange String

derive instance Newtype RawVersionRange _
derive newtype instance Eq RawVersionRange
derive newtype instance Ord RawVersionRange
