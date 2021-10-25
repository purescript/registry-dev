module Registry.Scripts.LegacyImport.ManifestFields where

import Registry.Prelude

import Data.String.NonEmpty (NonEmptyString)

type ManifestFields =
  { license :: Maybe (NonEmptyArray NonEmptyString)
  , dependencies :: Object String
  , devDependencies :: Object String
  }
