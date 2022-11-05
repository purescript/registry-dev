-- | Implementation of the `PackageIndex` data type as described in the registry
-- | spec. The package index records all packages in the registry along with the
-- | manifests associated with each version of the package.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#36-package-index
-- |
-- | The package index is published in the registry-index repository:
-- | https://github.com/purescript/registry-index
module Registry.PackageIndex where

import Prelude

import Control.Bind (bindFlipped)
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Graph (Graph)
import Data.Graph as Graph
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Node.Path (FilePath)
import Node.Path as Path
import Partial.Unsafe (unsafeCrashWith)
import Registry.Manifest (Manifest(..))
import Registry.Manifest as Manifest
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Version (Version)

-- | An index of package manifests, keyed by package name and version. The index
-- | is correct by construction: manifests only list dependencies on package
-- | versions which are also in the index.
newtype PackageIndex = PackageIndex (Map PackageName (Map Version Manifest))

derive instance Eq PackageIndex

-- | Create an empty `PackageIndex`
empty :: PackageIndex
empty = PackageIndex Map.empty

-- | Convert a package index into a `Map`
toMap :: PackageIndex -> Map PackageName (Map Version Manifest)
toMap (PackageIndex index) = index

-- | Look up a package version's manifest in the package index.
lookup :: PackageName -> Version -> PackageIndex -> Maybe Manifest
lookup name version (PackageIndex index) = Map.lookup name index >>= Map.lookup version

-- | Insert a new manifest into the package index, failing if the manifest
-- | indicates dependencies that cannot be satisfied. Dependencies are not
-- | satisfied if a) the package is not in the index or b) there are no versions
-- | in the indicated range.
insert :: Manifest -> PackageIndex -> Either (Map PackageName Range) PackageIndex
insert manifest@(Manifest { name, version, dependencies }) (PackageIndex index) = do
  let
    unsatisfied :: Map PackageName Range
    unsatisfied = Map.fromFoldable do
      Tuple dependency range <- Map.toUnfoldable dependencies
      case Map.lookup dependency index of
        Just versions | Array.any (Range.includes range) (Set.toUnfoldable (Map.keys versions)) -> []
        _ -> [ Tuple dependency range ]

  if Map.isEmpty unsatisfied then
    Right $ PackageIndex $ Map.insertWith Map.union name (Map.singleton version manifest) index
  else
    Left unsatisfied

type SolverStep = { progress :: Boolean, state :: SolverState }

type SolverState = { satisfied :: Map PackageName (Map Version Manifest), goals :: Array Goal }

type Goal = { manifest :: Manifest, unsolved :: Array PackageName }

-- | Construct a `PackageIndex` from a collection of manifests, if possible. If
-- | any manifests indicate dependencies on packages outside the collection,
-- | those failures are collected and reported.
-- |
-- | TODO: Can this be done by reusing the 'insert' function?
-- | TODO: This doesn't take into account missing package versions (it only
-- | looks at package names).
fromManifests :: Set Manifest -> Either (Map PackageName (Map Version (Array PackageName))) PackageIndex
fromManifests manifests =
  case solveGoals { satisfied: Map.empty, goals: buildGoals manifests } of
    result | Array.null result.goals -> Right $ PackageIndex result.satisfied
    result -> Left $ Map.fromFoldableWith Map.union do
      { manifest: Manifest { name, version }, unsolved } <- result.goals
      [ Tuple name (Map.singleton version unsolved) ]
  where
  -- A list of constraints that we need to satisfy, taken from the dependency
  -- array from each `Manifest`. We sort this by number of dependencies for
  -- performance: we want to trim the array as quickly as we can.
  --
  -- Invariant: The input manifests are in a Set, therefore the entries in the
  -- constraint list are unique.
  buildGoals :: Set Manifest -> Array Goal
  buildGoals inputs = Array.sortWith (_.unsolved >>> Array.length) do
    manifest@(Manifest { dependencies }) <- Set.toUnfoldable inputs
    let unsolved = Set.toUnfoldable (Map.keys dependencies)
    [ { manifest, unsolved } ]

  -- Solve goals until no more progress can be made.
  solveGoals :: SolverState -> SolverState
  solveGoals initialState = do
    let
      -- On each step we verify whether the given goal (ie. package) is
      -- satisfied by checking whether all its dependencies are already solved.
      -- If so, then we consider this goal also satisfied. If not, we add all
      -- unsolved dependencies to the list of remaining goals.
      runStep :: SolverStep -> Goal -> SolverStep
      runStep { progress, state } { manifest, unsolved } = do
        case Array.filter (not <<< flip Map.member state.satisfied) unsolved of
          [] -> do
            let { name, version } = un Manifest manifest
            { progress: true
            , state: state { satisfied = Map.insertWith Map.union name (Map.singleton version manifest) state.satisfied }
            }
          remaining ->
            { progress
            , state: state { goals = state.goals <> [ { manifest, unsolved: remaining } ] }
            }

      initialStep :: SolverStep
      initialStep = { progress: false, state: initialState { goals = [] } }

      runSteps :: SolverStep -> Array Goal -> SolverStep
      runSteps = Array.foldl runStep

    -- Go through each constraint, recording satisfied manifest dependencies
    -- and updating remaining goals. If we could not make progress then we
    -- terminate, but otherwise we continue.
    case runSteps initialStep initialState.goals of
      result | not result.progress -> result.state
      result -> solveGoals result.state

-- | Topologically sort a package index by package dependencies, where manifests
-- | in the resulting array only depend on manifests earlier in the array.
toSortedArray :: PackageIndex -> Array Manifest
toSortedArray (PackageIndex index) =
  Array.fromFoldable
    $ List.reverse
    $ List.mapMaybe (flip Graph.lookup graph)
    $ Graph.topologicalSort graph
  where
  graph :: Graph (Tuple PackageName Version) Manifest
  graph = do
    Graph.fromMap
      $ Map.fromFoldable
      $ map (map resolveDependencies)
      $ Array.foldMap flatten
      $ map (map Map.toUnfoldable) (Map.toUnfoldable index)

  flatten :: Tuple PackageName (Array (Tuple Version Manifest)) -> Array (Tuple (Tuple PackageName Version) Manifest)
  flatten (Tuple packageName versions) = do
    Tuple version manifest <- versions
    pure $ Tuple (Tuple packageName version) manifest

  resolveDependencies :: Manifest -> Tuple Manifest (List (Tuple PackageName Version))
  resolveDependencies manifest@(Manifest { dependencies }) = do
    let deps = List.fromFoldable $ bindFlipped resolveDependency $ Map.toUnfoldable dependencies
    Tuple manifest deps

  resolveDependency :: Tuple PackageName Range -> Array (Tuple PackageName Version)
  resolveDependency (Tuple dependency _) = do
    let allVersions = map (Map.keys >>> Set.toUnfoldable) index
    depVersions <- Maybe.maybe [] Array.singleton (Map.lookup dependency allVersions)
    version <- depVersions
    pure $ Tuple dependency version

-- | Calculate the directory containing this package in the registry index,
-- | using the following format:
-- |
-- | * Packages with 1 character names are placed in a directory named 1.
-- |
-- | * Packages with 2 character names are placed in a directory named 2.
-- |
-- | * Packages with 3 character names are placed in the directory 3/{first-character}
-- |   where {first-character} is the first character of the package name.
-- |
-- | * All other packages are stored in directories named {first-two}/{second-two}
-- |   where the top directory is the first two characters of the package name,
-- |   and the next subdirectory is the third and fourth characters of the
-- |   package name. For example, prelude would be stored in the 'pr/el'
-- |   directory.
-- |
-- | Format follows that used by Cargo in crates.io: https://github.com/rust-lang/crates.io-index
-- | As documented in the Cargo book: https://doc.rust-lang.org/cargo/reference/registries.html#index-format
packageEntryDirectory :: PackageName -> FilePath
packageEntryDirectory = PackageName.print >>> \name -> case String.length name of
  -- Package names are validated to be non-empty strings, so this case is not
  -- reachable unless the package name was unsafely coerced.
  0 -> unsafeCrashWith "Unexpected empty PackageName"
  1 -> "1"
  2 -> "2"
  3 -> Path.concat [ "3", String.take 1 name ]
  _ -> Path.concat [ String.take 2 name, String.take 2 (String.drop 2 name) ]

-- | Calculate the full file path for a given package name. For example, the
-- | 'prelude' package would be stored at 'pr/el/prelude'.
packageEntryFilePath :: PackageName -> FilePath
packageEntryFilePath name = Path.concat [ packageEntryDirectory name, PackageName.print name ]

parseEntry :: String -> Either String (Array Manifest)
parseEntry entry = do
  let split = String.split (String.Pattern "\n") <<< String.trim
  jsonArray <- traverse Argonaut.Parser.jsonParser (split entry)
  traverse (lmap CA.printJsonDecodeError <<< CA.decode Manifest.codec) jsonArray

printEntry :: Array Manifest -> String
printEntry =
  String.joinWith "\n"
    <<< map (Argonaut.stringify <<< CA.encode Manifest.codec)
    <<< Array.sortBy (comparing (_.version <<< un Manifest))
