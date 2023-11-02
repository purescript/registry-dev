-- | A module describing the output of 'purs graph' along with some helper
-- | functions for working with the graph.
module Registry.PursGraph where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (bimap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Profunctor as Profunctor
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Node.Path (FilePath)
import Registry.Internal.Codec as Internal.Codec
import Registry.PackageName (PackageName)

-- | A graph of the dependencies between modules, discovered by the purs
-- | compiler from a set of source files.
type PursGraph = Map ModuleName PursGraphNode

pursGraphCodec :: JsonCodec PursGraph
pursGraphCodec = Internal.Codec.strMap "PursGraph" (Just <<< ModuleName) (un ModuleName) pursGraphNodeCodec

type PursGraphNode =
  { depends :: Array ModuleName
  , path :: FilePath
  }

pursGraphNodeCodec :: JsonCodec PursGraphNode
pursGraphNodeCodec = CA.Record.object "PursGraphNode"
  { depends: CA.array moduleNameCodec
  , path: CA.string
  }

-- | A module name string from a 'purs graph' invocation.
newtype ModuleName = ModuleName String

derive instance Newtype ModuleName _
derive instance Eq ModuleName
derive instance Ord ModuleName

moduleNameCodec :: JsonCodec ModuleName
moduleNameCodec = Profunctor.wrapIso ModuleName CA.string

type AssociatedError = { module :: ModuleName, path :: FilePath, error :: String }

-- | Given a function to parse the `path` component of `purs graph`, associate
-- | all associate all modules in the groph with their package names.
associateModules :: (FilePath -> Either String PackageName) -> PursGraph -> Either (NonEmptyArray AssociatedError) (Map ModuleName PackageName)
associateModules parse graph = do
  let
    parsed :: Array (Either AssociatedError (Tuple ModuleName PackageName))
    parsed = Map.toUnfoldableUnordered graph # map \(Tuple moduleName { path }) -> parse path # bimap
      (\error -> { module: moduleName, path, error })
      (\packageName -> Tuple moduleName packageName)

    separated :: { errors :: Array AssociatedError, values :: Array (Tuple ModuleName PackageName) }
    separated = parsed # Array.foldMap case _ of
      Left err -> { errors: [ err ], values: [] }
      Right tup -> { errors: [], values: [ tup ] }

  case NonEmptyArray.fromArray separated.errors of
    Nothing -> pure $ Map.fromFoldable separated.values
    Just errors -> Left errors

-- | Find direct dependencies of the given module, according to the given graph.
directDependencies :: ModuleName -> PursGraph -> Maybe (Set ModuleName)
directDependencies name = map (Set.fromFoldable <<< _.depends) <<< Map.lookup name

-- | Find all dependencies of the given module, according to the given graph.
allDependencies :: ModuleName -> PursGraph -> Maybe (Set ModuleName)
allDependencies start graph = map Set.fromFoldable (getDependencies start)
  where
  getDependencies name =
    map _.depends (Map.lookup name graph) >>= case _ of
      [] -> pure []
      directs -> do
        let nextDeps = map Array.concat (traverse getDependencies directs)
        nextDeps <> Just directs
