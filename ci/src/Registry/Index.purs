module Registry.Index where

import Registry.Prelude

import Foreign.GitHub as GitHub
import Registry.Schema (Manifest)

type PackageName = String

type RegistryIndex = Map PackageName (Map GitHub.Tag Manifest)