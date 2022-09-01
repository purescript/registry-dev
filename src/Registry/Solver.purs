module Registry.Solver where

import Registry.Prelude

import Control.Monad.Error.Class (class MonadError, catchError)
import Control.Monad.Reader (ask)
import Control.Monad.State (class MonadState, get, modify_, put)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty (foldMap1)
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex, traverseWithIndex_)
import Data.Generic.Rep as Generic
import Data.Map as Map
import Data.Newtype (alaF)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Set as Set
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Range, Version, intersect, printRange, rangeIncludes)
import Registry.Version as Version
import Uncurried.RWSE (RWSE, runRWSE)

type Dependencies = Map PackageName (Map Version (Map PackageName Range))

data SolverPosition
  = SolveRoot
  | Solving PackageName (NonEmptyArray Version) SolverPosition

derive instance Eq SolverPosition
derive instance Ord SolverPosition
derive instance Generic.Generic SolverPosition _

instance Show SolverPosition where
  show a = genericShow a

printSolverPosition :: SolverPosition -> String
printSolverPosition = case _ of
  SolveRoot -> ""
  Solving name versions pos -> Array.fold
    [ " while solving "
    , PackageName.print name
    , "@"
    , intercalateMap ", " Version.printVersion versions
    , printSolverPosition pos
    ]

data SolverError
  = NoVersionsInRange PackageName (Set Version) Range SolverPosition
  | VersionNotInRange PackageName Version Range SolverPosition
  | DisjointRanges PackageName Range SolverPosition Range SolverPosition

groupErrors :: NonEmptyArray SolverError -> NonEmptyArray SolverError
groupErrors = map NEA.nubEq $ fromGroup <=< NEA.groupAllBy grouping
  where
  grouping (NoVersionsInRange p1 v1 r1 _) (NoVersionsInRange p2 v2 r2 _) =
    compare p1 p2 <> compare v1 v2 <> compare (printRange r1) (printRange r2)
  grouping (NoVersionsInRange _ _ _ _) _ = LT
  grouping _ (NoVersionsInRange _ _ _ _) = GT
  grouping (VersionNotInRange p1 v1 r1 _) (VersionNotInRange p2 v2 r2 _) =
    compare p1 p2 <> compare v1 v2 <> compare (printRange r1) (printRange r2)
  grouping (VersionNotInRange _ _ _ _) _ = LT
  grouping _ (VersionNotInRange _ _ _ _) = GT
  grouping (DisjointRanges p1 r1 s1 q1 _) (DisjointRanges p2 r2 s2 q2 _) =
    compare p1 p2 <> compare (printRange r1) (printRange r2) <> compare s1 s2 <> compare (printRange q1) (printRange q2)

  fromGroup es = setPosition (NEA.head es) $ groupPositions $ map getPosition es

  getPosition (NoVersionsInRange _ _ _ p) = p
  getPosition (VersionNotInRange _ _ _ p) = p
  getPosition (DisjointRanges _ _ _ _ p) = p

  setPosition (NoVersionsInRange p v r _) = map $ NoVersionsInRange p v r
  setPosition (VersionNotInRange p v r _) = map $ VersionNotInRange p v r
  setPosition (DisjointRanges p r s q _) = map $ DisjointRanges p r s q

groupPositions :: NonEmptyArray SolverPosition -> NonEmptyArray SolverPosition
groupPositions = fromGroup <=< NEA.groupAllBy grouping
  where
  grouping SolveRoot SolveRoot = EQ
  grouping SolveRoot _ = LT
  grouping _ SolveRoot = GT
  grouping (Solving p1 _ s1) (Solving p2 _ s2) =
    compare p1 p2 <> compare s1 s2

  fromGroup es = setVersion es $ Array.nub $ getVersions =<< NEA.toArray es

  getVersions SolveRoot = empty
  getVersions (Solving _ v _) = NEA.toArray v

  setVersion os = NEA.fromArray >>>
    maybe os case NEA.head os, _ of
      SolveRoot, _ -> pure SolveRoot
      Solving p _ s, v -> pure (Solving p v s)

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

newtype CollectErrors :: Type -> Type
newtype CollectErrors a = CollectErrors (Solver a)

derive instance Newtype (CollectErrors a) _

instance Semigroup (CollectErrors a) where
  append (CollectErrors fa) (CollectErrors fb) = CollectErrors do
    s <- get
    catchError fa \e1 -> do
      -- if unrelated, drop the conflict from state, advance
      put s
      catchError fb \e2 -> do
        throwError (e1 <> e2)

oneOfMap1 :: forall a b. (a -> Solver b) -> NonEmptyArray a -> Solver b
oneOfMap1 = alaF CollectErrors foldMap1

type ValidationError =
  { name :: PackageName
  , range :: Range
  , version :: Maybe Version
  }

validate :: Map PackageName Range -> Solved -> Either (NonEmptyArray ValidationError) Unit
validate index sols = maybe (Right unit) Left $ NEA.fromArray
  $ index
  # foldMapWithIndex \name range ->
      case Map.lookup name sols of
        Just version | rangeIncludes range version -> empty
        version -> pure { name, range, version }

solve :: Dependencies -> Map PackageName Range -> Either (NonEmptyArray SolverError) Solved
solve index pending = lmap groupErrors
  case runRWSE index { pending: map (Tuple SolveRoot) pending, solved: Map.empty } (exploreGoals true) of
    _ /\ r /\ _ -> r

solveAndValidate :: Dependencies -> Map PackageName Range -> Either (NonEmptyArray SolverError) Solved
solveAndValidate index pending = do
  sols <- solve index pending
  case validate pending sols of
    Left es -> Left $ es <#> \r ->
      case r.version of
        Nothing -> NoVersionsInRange r.name Set.empty r.range SolveRoot
        Just version -> VersionNotInRange r.name version r.range SolveRoot
    Right _ -> Right sols

exploreGoals :: Boolean -> Solver Solved
exploreGoals recover =
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
            addVersion pos name version *> exploreGoals false
        const act $ catchError act \e1 -> do
          when recover $ void $
            catchError (exploreGoals recover) \e2 -> do
              throwError (e1 <> e2)
          throwError e1

addVersion :: SolverPosition -> PackageName -> (Tuple Version (Map PackageName Range)) -> Solver Unit
addVersion pos name (Tuple version deps) = do
  modify_ \s -> s { solved = Map.insert name version s.solved }
  traverseWithIndex_ (addConstraint (Solving name (pure version) pos)) deps

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
