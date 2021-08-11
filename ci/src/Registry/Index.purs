module Registry.Index where

import Registry.Prelude

import Foreign.GitHub as GitHub
import Registry.PackageName (PackageName)
import Registry.Schema (Manifest)

type RegistryIndex = Map PackageName (Map GitHub.Tag Manifest)
