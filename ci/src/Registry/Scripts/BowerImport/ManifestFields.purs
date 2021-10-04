module Registry.Scripts.BowerImport.ManifestFields where

import Registry.Prelude

type ManifestFields =
  { license :: Maybe (NonEmptyArray String)
  , dependencies :: Object String
  , devDependencies :: Object String
  }
