module Registry.Solver where

import Registry.Prelude

import Control.Monad.Error.Class (class MonadError, catchError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify_, put)
import Data.Array as Array
import Data.Array.NonEmpty (foldMap1)
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Generic.Rep as Generic
import Data.Map as Map
import Data.Newtype (alaF)
import Data.Semigroup.Foldable (intercalateMap)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Range, Version, intersect, rangeIncludes)
import Registry.Version as Version
import Uncurried.RWSE (RWSE, runRWSE)

type Dependencies = Map PackageName (Map Version (Map PackageName Range))

data SolverPosition
  = SolveRoot
  | Solving PackageName Version SolverPosition

derive instance Eq SolverPosition
derive instance Ord SolverPosition
derive instance Generic.Generic SolverPosition _

instance Show SolverPosition where
  show a = genericShow a

printSolverPosition :: SolverPosition -> String
printSolverPosition = case _ of
  SolveRoot -> ""
  Solving name version pos -> Array.fold
    [ " while solving "
    , PackageName.print name
    , "@"
    , Version.printVersion version
    , printSolverPosition pos
    ]

data SolverError
  = NoVersionsInRange PackageName (Set Version) Range SolverPosition
  | VersionNotInRange PackageName Version Range SolverPosition
  | DisjointRanges PackageName Range SolverPosition Range SolverPosition

derive instance Eq SolverError
derive instance Generic.Generic SolverError _

instance Show SolverError where
  show a = genericShow a

printSolverError :: SolverError -> String
printSolverError = case _ of
  NoVersionsInRange name versions range pos -> Array.fold
    [ "Package index contained no versions for "
    , PackageName.print name
    , " in the range "
    , Version.printRange range
    , " (existing versions: "
    , maybe "none" (intercalateMap ", " Version.printVersion) (NEA.fromFoldable versions)
    , ")"
    , printSolverPosition pos
    ]
  VersionNotInRange name version range pos -> Array.fold
    [ "Committed to "
    , PackageName.print name
    , "@"
    , Version.printVersion version
    , " but the range "
    , Version.printRange range
    , " was also required"
    , printSolverPosition pos
    ]
  DisjointRanges name range1 pos1 range2 pos2 -> Array.fold
    [ "Committed to "
    , PackageName.print name
    , " in range "
    , Version.printRange range1
    , printSolverPosition pos1
    , " but the range "
    , Version.printRange range2
    , " was also required"
    , printSolverPosition pos2
    ]

type Solver = RWSE Dependencies Unit State (NonEmptyArray SolverError)

type Goals = Map PackageName (Tuple SolverPosition Range)
type Solved = Map PackageName Version
type State =
  { pending :: Goals
  , solved :: Map PackageName Version
  }

newtype CollectErrors :: (Type -> Type) -> Type -> Type
newtype CollectErrors f a = CollectErrors (f a)

derive instance Newtype (CollectErrors f a) _

instance (Semigroup e, MonadError e f) => Semigroup (CollectErrors f a) where
  append (CollectErrors fa) (CollectErrors fb) = CollectErrors $
    catchError fa \e1 ->
      catchError fb \e2 ->
        throwError (e1 <> e2)

oneOfMap1 :: forall e f a b. Semigroup e => MonadError e f => (a -> f b) -> NonEmptyArray a -> f b
oneOfMap1 = alaF CollectErrors foldMap1

solve :: Dependencies -> Map PackageName Range -> Either (NonEmptyArray SolverError) Solved
solve index pending =
  case runRWSE index { pending: map (Tuple SolveRoot) pending, solved: Map.empty } exploreGoals of
    _ /\ r /\ _ -> r

exploreGoals :: Solver Solved
exploreGoals =
  get >>= \goals@{ pending, solved } ->
    case Map.findMin pending of
      Nothing ->
        pure solved

      Just { key: name, value: Tuple pos constraint } -> do
        let otherPending = Map.delete name pending
        let goals' = goals { pending = otherPending }
        put goals'
        versions <- getRelevantVersions pos name constraint
        let
          act = versions # oneOfMap1 \version ->
            addVersion pos name version *> exploreGoals
        catchError act \e1 -> do
          _ <- catchError exploreGoals \e2 -> do
            throwError (e1 <> e2)
          throwError e1

addVersion :: SolverPosition -> PackageName -> (Tuple Version (Map PackageName Range)) -> Solver Unit
addVersion pos name (Tuple version deps) = do
  modify_ \s -> s { solved = Map.insert name version s.solved }
  traverseWithIndex_ (addConstraint (Solving name version pos)) deps

addConstraint :: SolverPosition -> PackageName -> Range -> Solver Unit
addConstraint pos name newConstraint = do
  goals@{ pending, solved } <- get
  case Map.lookup name solved of
    Just version ->
      if rangeIncludes newConstraint version then pure unit
      else throwError $ pure $ VersionNotInRange name version newConstraint pos

    Nothing ->
      case Map.lookup name pending of
        Nothing -> put $ goals { pending = Map.insert name (Tuple pos newConstraint) pending }

        Just (Tuple oldPos oldConstraint) ->
          case intersect oldConstraint newConstraint of
            Nothing ->
              throwError $ pure $ DisjointRanges name oldConstraint oldPos newConstraint pos

            Just mergedConstraint ->
              if oldConstraint == mergedConstraint then pure unit
              else put $ goals { pending = Map.insert name (Tuple pos mergedConstraint) pending }

getRelevantVersions :: SolverPosition -> PackageName -> Range -> Solver (NonEmptyArray (Tuple Version (Map PackageName Range)))
getRelevantVersions pos name constraint = do
  index <- ask
  let
    versions =
      Map.lookup name index # foldMap do
        -- Put newest versions first
        Map.toUnfoldable >>> Array.reverse
          >>> Array.filter (fst >>> rangeIncludes constraint)
          >>> NEA.fromArray
  case versions of
    Just vs -> pure vs
    Nothing ->
      throwError $ pure $ NoVersionsInRange name (Map.lookup name index # foldMap Map.keys) constraint pos
