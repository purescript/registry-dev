module Registry.Solver where

import Registry.Prelude
import Registry.Version (Range, Version, intersect, rangeIncludes)

import Control.Monad.Reader (class MonadAsk, ask, asks)
import Control.Monad.State (class MonadState, get, put)
import Control.Plus (class Alt, class Plus, empty)
import Data.Foldable (foldMap, oneOfMap)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Set as Set
import Registry.Index (RegistryIndex)
import Registry.PackageName (PackageName)

newtype SolverT :: (Type -> Type) -> Type -> Type
newtype SolverT m a =
  Solver
  (
    forall b.
      -- `ask`
      RegistryIndex
      -- `get`
      -> State
      -- `empty`
      -> (State -> m b)
      -- `pure`/`put`
      -> ((State -> m b) -> State -> a -> m b)
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

instance monadAskSolver :: MonadAsk RegistryIndex (SolverT m) where
  ask = Solver \r s back ok -> ok back s r
instance monadStateSolver :: MonadState State (SolverT m) where
  state f = Solver \_ s back ok ->
    let Tuple a s' = f s
    in ok back s' a

instance altSolver :: Alt (SolverT m) where
  alt (Solver c1) (Solver c2) = Solver \r s back ok ->
    c1 r s (\s' -> c2 r s' back ok) ok
instance plusSolver :: Plus (SolverT m) where
  empty = Solver \_ s back _ -> back s

type Goals = Map PackageName Range
type Solved = Map PackageName Version
type State =
  { pending :: Goals
  , solved :: Map PackageName Version
  }


exploreGoals :: Solver Solved
exploreGoals =
  get >>= \{ pending, solved } ->
    case Map.findMin pending of
      Nothing ->
        pure solved

      Just { key: name, value: constraint } -> do
        let otherPending = Map.delete name pending
        let goals1 = { pending: otherPending, solved }
        put goals1
        oneOfMap (addVersion name) =<< getRelevantVersions name constraint
        exploreGoals


addVersion :: PackageName -> Version -> Solver Unit
addVersion name version = do
  deps <- getConstraints name version
  traverseWithIndex_ addConstraint deps

addConstraint :: PackageName -> Range -> Solver Unit
addConstraint name newConstraint = do
  goals@{ pending, solved } <- get
  case Map.lookup name solved of
    Just version ->
      if rangeIncludes newConstraint version
      then pure unit
      else empty

    Nothing ->
      case Map.lookup name pending of
        Nothing ->
          put $ goals { pending = Map.insert name newConstraint pending }

        Just oldConstraint ->
          case intersect oldConstraint newConstraint of
            Nothing ->
              empty

            Just mergedConstraint ->
              if oldConstraint == mergedConstraint
              then pure unit
              else put $ goals { pending = Map.insert name mergedConstraint pending }

getRelevantVersions :: PackageName -> Range -> Solver (Set Version)
getRelevantVersions name constraint =
  asks \index ->
    Map.lookup name index # foldMap do
      Map.keys >>> Set.filter (rangeIncludes constraint)

getConstraints :: PackageName -> Version -> Solver (Map PackageName Range)
getConstraints pkg vsn =
  ask >>= \index ->
    case Map.lookup pkg index >>= Map.lookup vsn of
      Just cs ->
        pure (unwrap cs).dependencies

      Nothing ->
        empty
