module Registry.Solver where

import Registry.Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.State (class MonadState, get, modify_, put)
import Control.Plus (class Alt)
import Data.Array as Array
import Data.Array.NonEmpty (foldMap1)
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Generic.Rep as Generic
import Data.Map as Map
import Data.Newtype (alaF, unwrap)
import Data.Semigroup.Foldable (intercalateMap)
import Registry.PackageName (PackageName)
import Registry.Version (Range, Version, intersect, rangeIncludes)

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
  Solving name version pos -> " while solving " <> show name <> "@" <> show version <> show pos

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
  NoVersionsInRange name versions range pos ->
    "Package index contained no versions for " <> show name <> " in the range " <> show range <> " (existing versions: " <> shownVersions <> ")" <> show pos
    where
    shownVersions = maybe "none" (intercalateMap ", " show) (NEA.fromFoldable versions)
  VersionNotInRange name version range pos ->
    "Committed to " <> show name <> "@" <> show version <> " but the range " <> show range <> " was also required" <> show pos
  DisjointRanges name range1 pos1 range2 pos2 ->
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
      -> (NonEmptyArray SolverError -> m b)
      -- `pure`/`put`
      -> ((NonEmptyArray SolverError -> m b) -> State -> a -> m b)
      -> m b
    )

type Solver = SolverT Identity

instance Functor (SolverT m) where
  map f (Solver c) = Solver \r s back ok -> c r s back (\back' s' a -> ok back' s' (f a))

instance Apply (SolverT m) where
  apply = ap

instance Applicative (SolverT m) where
  pure a = Solver \_ s back ok -> ok back s a

instance Bind (SolverT m) where
  bind (Solver c) f = Solver \r s back ok ->
    c r s back \back' s' a ->
      case f a of
        Solver c' -> c' r s' back' ok

instance Monad (SolverT m)

instance MonadAsk Dependencies (SolverT m) where
  ask = Solver \r s back ok -> ok back s r

instance MonadState State (SolverT m) where
  state f = Solver \_ s back ok ->
    let
      Tuple a s' = f s
    in
      ok back s' a

instance Alt (SolverT m) where
  alt (Solver c1) (Solver c2) = Solver \r s back ok ->
    c1 r s (\_ -> c2 r s back ok) ok

instance MonadThrow (NonEmptyArray SolverError) (SolverT m) where
  throwError e = Solver \_ _ back _ -> back e

instance MonadError (NonEmptyArray SolverError) (SolverT m) where
  catchError (Solver ma) recover = Solver \r s back ok ->
    let
      back' e = case recover e of
        Solver ma' ->
          ma' r s back ok
    in
      ma r s back' ok

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

moreErrors
  :: forall e f a b
   . Discard a
  => Semigroup e
  => MonadError e f
  => f a
  -> f b
  -> f b
moreErrors a b = do
  catchError a \e1 -> do
    _ <- catchError b \e2 -> throwError (e1 <> e2)
    throwError e1
  b

solve :: Dependencies -> Map PackageName Range -> Either (NonEmptyArray SolverError) Solved
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
        put $ goals { pending = Map.delete name pending }
        moreErrors
          do oneOfMap1 (addVersion pos name) =<< getRelevantVersions pos name constraint
          do exploreGoals

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
