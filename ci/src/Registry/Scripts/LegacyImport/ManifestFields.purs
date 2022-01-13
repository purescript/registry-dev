module Registry.Scripts.LegacyImport.ManifestFields where

import Registry.Prelude

import Data.String.NonEmpty (NonEmptyString)
import Registry.Scripts.LegacyImport.Error (RawPackageName, RawVersion)

type ManifestFields =
  { license :: Maybe (NonEmptyArray NonEmptyString)
  , description :: Maybe String
  , dependencies :: Map RawPackageName RawVersion
  , devDependencies :: Map RawPackageName RawVersion
  }
