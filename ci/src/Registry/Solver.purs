module Registry.Solver where

import Registry.Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.State (class MonadState, get, modify_, put)
import Control.Plus (class Alt)
import Data.Array as Array
import Data.Array.NonEmpty (foldMap1)
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map as Map
import Data.Monoid.Alternate (Alternate(..))
import Data.Newtype (alaF, unwrap)
import Data.Semigroup.Foldable (intercalateMap)
import Registry.PackageName (PackageName)
import Registry.Version (Range, Version, intersect, rangeIncludes)

type Dependencies = Map PackageName (Map Version (Map PackageName Range))

data SolverPosition
  = SolveRoot
  | Solving PackageName Version SolverPosition

derive instance eqSolverPosition :: Eq SolverPosition
derive instance ordSolverPosition :: Ord SolverPosition

instance showSolverPosition :: Show SolverPosition where
  show SolveRoot = ""
  show (Solving name version pos) = " while solving " <> show name <> "@" <> show version <> show pos

data SolverError
  = NoVersionsInRange PackageName (Set Version) Range SolverPosition
  | VersionNotInRange PackageName Version Range SolverPosition
  | DisjointRanges PackageName Range SolverPosition Range SolverPosition

derive instance eqSolverError :: Eq SolverError

instance showSolverError :: Show SolverError where
  show (NoVersionsInRange name versions range pos) =
    "Package index contained no versions for " <> show name <> " in the range " <> show range <> " (existing versions: " <> shownVersions <> ")" <> show pos
    where
    shownVersions = maybe "none" (intercalateMap ", " show) (NEA.fromFoldable versions)
  show (VersionNotInRange name version range pos) =
    "Committed to " <> show name <> "@" <> show version <> " but the range " <> show range <> " was also required" <> show pos
  show (DisjointRanges name range1 pos1 range2 pos2) =
    "Committed to " <> show name <> " in range " <> show range1 <> show pos1 <> " but the range " <> show range2 <> " was also required" <> show pos2

newtype SolverT :: (Type -> Type) -> Type -> Type
newtype SolverT m a =
  Solver
    ( forall b
       .
      -- `ask`
      Dependencies
      -- `get`
      -> State
      -- `empty`
      -> (SolverError -> m b)
      -- `pure`/`put`
      -> ((SolverError -> m b) -> State -> a -> m b)
      -> m b
    )

type Solver = SolverT Identity

instance functorSolver :: Functor (SolverT m) where
  map f (Solver c) = Solver \r s back ok -> c r s back (\back' s' a -> ok back' s' (f a))

instance applySolver :: Apply (SolverT m) where
  apply = ap

instance applicativeSolver :: Applicative (SolverT m) where
  pure a = Solver \_ s back ok -> ok back s a

instance bindSolver :: Bind (SolverT m) where
  bind (Solver c) f = Solver \r s back ok ->
    c r s back \back' s' a ->
      case f a of
        Solver c' -> c' r s' back' ok

instance monadSolver :: Monad (SolverT m)

instance monadAskSolver :: MonadAsk Dependencies (SolverT m) where
  ask = Solver \r s back ok -> ok back s r

instance monadStateSolver :: MonadState State (SolverT m) where
  state f = Solver \_ s back ok ->
    let
      Tuple a s' = f s
    in
      ok back s' a

instance altSolver :: Alt (SolverT m) where
  alt (Solver c1) (Solver c2) = Solver \r s back ok ->
    c1 r s (\_ -> c2 r s back ok) ok

instance monadThrowSolver :: MonadThrow SolverError (SolverT m) where
  throwError e = Solver \_ _ back _ -> back e

type Goals = Map PackageName (Tuple SolverPosition Range)
type Solved = Map PackageName Version
type State =
  { pending :: Goals
  , solved :: Map PackageName Version
  }

oneOfMap1 :: forall f a b. Alt f => (a -> f b) -> NonEmptyArray a -> f b
oneOfMap1 = alaF Alternate foldMap1

solve :: Dependencies -> Map PackageName Range -> Either SolverError Solved
solve index pending = unwrap $ case exploreGoals of
  Solver c ->
    c index { pending: map (Tuple SolveRoot) pending, solved: Map.empty }
      (pure <<< Left)
      (\_ _ solved -> pure (pure solved))

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
        _ <- oneOfMap1 (addVersion pos name) =<< getRelevantVersions pos name constraint
        exploreGoals

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
      else throwError $ VersionNotInRange name version newConstraint pos

    Nothing ->
      case Map.lookup name pending of
        Nothing -> put $ goals { pending = Map.insert name (Tuple pos newConstraint) pending }

        Just (Tuple oldPos oldConstraint) ->
          case intersect oldConstraint newConstraint of
            Nothing ->
              throwError $ DisjointRanges name oldConstraint oldPos newConstraint pos

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
      throwError $ NoVersionsInRange name (Map.lookup name index # foldMap Map.keys) constraint pos
