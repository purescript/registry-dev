module Registry.PackageGraph where

import Registry.Prelude

import Data.Foldable (foldMap)
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List as List
import Data.Map as Map
import Data.Semigroup.First (First(..))
import Data.Set as Set
import Foreign.Object as Object
import Foreign.SemVer (Range, SemVer)
import Foreign.SemVer as SemVer
import Registry.Index (RegistryIndex)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest)
import Safe.Coerce (coerce)

type PackageGraph = Graph (Tuple PackageName SemVer) Manifest

-- We will store `maxSatisfying` for all dependencies
toPackageGraph :: RegistryIndex -> PackageGraph
toPackageGraph index =
  Graph.fromMap
    $ (Map.fromFoldable :: _ -> Map (Tuple PackageName SemVer) (Tuple Manifest (List (Tuple PackageName SemVer))))
    -- Need Array (Tuple (Tuple PackageName SemVer) (Tuple Manifest (List (Tuple PackageName SemVer))))
    $ map (map resolveDependencies)
    -- Array (Tuple (Tuple PackageName SemVer) Manifest)
    $ foldMap flatten
    -- Array (Tuple PackageName (Array (Tuple SemVer Manifest)))
    $ map (map (Map.toUnfoldable :: Map _ _ -> Array _))
    $ (Map.toUnfoldable index :: Array (Tuple PackageName (Map SemVer Manifest)))
  where
  allVersions :: Map PackageName (Array SemVer)
  allVersions = map (Map.keys >>> Set.toUnfoldable) index

  flatten
    :: Tuple PackageName (Array (Tuple SemVer Manifest))
    -> Array (Tuple (Tuple PackageName SemVer) Manifest)
  flatten (Tuple packageName versions) = do
    Tuple version manifest <- versions
    pure $ Tuple (Tuple packageName version) manifest

  resolveDependencies
    :: Manifest
    -> Tuple Manifest (List (Tuple PackageName SemVer))
  resolveDependencies manifest = do
    let
      deps =
        List.fromFoldable
          -- Can we crash if we can't find a satisfying range?
          $ map resolveDependency
          $ map (lmap unsafeParsePackageName)
          $ (Object.toUnfoldable :: Object _ -> Array (Tuple String _))
          -- Assuming that different targets have the same version ranges for dependencies
          $ foldMap (_.dependencies >>> (coerce :: Object Range -> Object (First Range)))
          -- TODO: Do we only care about `lib`?
          $ Object.values manifest.targets
    Tuple manifest deps

  resolveDependency :: Tuple PackageName (First Range) -> Tuple PackageName SemVer
  resolveDependency (Tuple dependency (First range)) = Tuple dependency $ unsafePartial $ fromJust do
    depVersions <- Map.lookup dependency allVersions
    SemVer.maxSatisfying depVersions range

  unsafeParsePackageName :: String -> PackageName
  unsafeParsePackageName = fromRight' (\_ -> unsafeCrashWith "PackageName must parse") <<< PackageName.parse

inOrder :: RegistryIndex -> List Manifest
inOrder index = do
  let graph = toPackageGraph index

  List.mapMaybe (flip Graph.lookup graph)
    $ Graph.topologicalSort graph
