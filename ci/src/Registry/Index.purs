module Registry.Index where

import Registry.Prelude

import Foreign.GitHub as GitHub
import Registry.Schema (Manifest)

{-

TODO @thomashoneyman:
- function from (PackageName, Version) -> Path
  Following the Cargo structure: https://github.com/rust-lang/crates.io-index
  Documentation for the format in https://doc.rust-lang.org/cargo/reference/registries.html#index-format
  Either we can have a folder for every package and then a single JSON file for every version
  or have a JSONLines (.jsonl) file for every package and a line for every version in there
- function to write a Registry.Schema.Manifest to such path
- function to read the whole `registry-index` into a RegistryIndex

-}

type PackageName = String

type RegistryIndex = Map PackageName (Map GitHub.Tag Manifest)