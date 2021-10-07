module Registry.PackageGraph where

import Registry.Prelude

import Data.Array as Array
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List as List
import Data.Map as Map
import Foreign.SemVer (SemVer)
import Registry.Index (RegistryIndex)
import Registry.PackageName (PackageName)
import Registry.Schema (Manifest)

type PackageGraph = Graph (Tuple PackageName SemVer) Manifest

-- We will store `maxSatisfying` for all dependencies
toPackageGraph :: RegistryIndex -> PackageGraph
toPackageGraph = ?a

inOrder :: RegistryIndex -> List Manifest
inOrder index = do
  let graph = toPackageGraph index

  List.mapMaybe (flip Graph.lookup graph)
    $ Graph.topologicalSort graph
