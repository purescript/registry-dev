module Registry.Solver where

import Registry.App.Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify_, put)
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty (foldMap1)
import Data.Array.NonEmpty as NEA
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex, traverseWithIndex_)
import Data.Map as Map
import Data.Newtype (alaF)
import Data.Semigroup.First (First(..))
import Data.Semigroup.Foldable (intercalateMap)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NES
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Version (Version)
import Registry.Version as Version
import Uncurried.RWSE (RWSE, runRWSE)

type Dependencies = Map PackageName (Map Version (Map PackageName Range))

data SolverPosition
  = SolveRoot
  | Solving PackageName (NonEmptyArray Version) SolverPosition

derive instance Eq SolverPosition
derive instance Ord SolverPosition

printSolverPosition :: SolverPosition -> String
printSolverPosition = case _ of
  SolveRoot -> ""
  Solving name versions pos -> Array.fold
    [ " while solving "
    , PackageName.print name
    , "@"
    , intercalateMap ", " Version.print versions
    , printSolverPosition pos
    ]

-- Find the shortest path from the initial dependencies to the desired specific
-- dependencies (package@version)
minimizeSolverPositions :: Dependencies -> Map PackageName Range -> Map PackageName (Set Version) -> Map (Tuple PackageName Version) (First SolverPosition)
minimizeSolverPositions index initialGoals errored = go Map.empty (map (NEA.singleton <<< Tuple SolveRoot) initialGoals)
  where
  toBeFound :: Set (Tuple PackageName Version)
  toBeFound = errored # foldMapWithIndex (Set.map <<< Tuple)

  go
    :: Map (Tuple PackageName Version) (First SolverPosition)
    -> Map PackageName (NonEmptyArray (Tuple SolverPosition Range))
    -> Map (Tuple PackageName Version) (First SolverPosition)
  -- Found enough, go home
  go found _ | toBeFound `Set.subset` Map.keys found = found
  -- Add relevant things from `currentGoals`, go to next layer
  go alreadyFound currentGoals =
    let
      foundHere = currentGoals
        # foldMapWithIndex \package -> foldMap \(Tuple pos range) ->
            Map.lookup package errored
              # foldMap (Set.filter (Range.includes range))
              # foldMap \version -> Map.singleton (Tuple package version) (First pos)

      nextGoals :: Map PackageName (NonEmptyArray (Tuple SolverPosition Range))
      nextGoals = currentGoals
        # foldMapWithIndex \package -> foldMap \(Tuple pos range) ->
            Map.lookup package index
              # maybe Map.empty (Map.filterWithKey (\v _ -> Range.includes range v))
              # foldMapWithIndex \version deps ->
                  NEA.singleton <<< Tuple (Solving package (pure version) pos) <$> deps
    in
      go (alreadyFound <> foundHere) nextGoals

data SolverError
  = NoVersionsInRange PackageName (Set Version) Range SolverPosition
  | VersionNotInRange PackageName (NonEmptySet Version) Range SolverPosition
  | DisjointRanges PackageName Range SolverPosition Range SolverPosition

derive instance Eq SolverError

-- Minimize the positions on the errors and group/deduplicate them
minimizeErrors :: Dependencies -> Map PackageName Range -> NonEmptyArray SolverError -> NonEmptyArray SolverError
minimizeErrors index goals errs = groupErrors (minimizePosition =<< errs)
  where
  collected = errs # foldMap case _ of
    NoVersionsInRange _ _ _ pos -> collectPackageVersion pos
    VersionNotInRange _ _ _ pos -> collectPackageVersion pos
    DisjointRanges _ _ p1 _ p2 ->
      collectPackageVersion p1 <> collectPackageVersion p2

  collectPackageVersion SolveRoot = mempty
  collectPackageVersion (Solving package version _) =
    foldMap (Map.singleton package <<< Set.singleton) version

  minimizePosition (NoVersionsInRange a b c pos) = NoVersionsInRange a b c <$> getMinimized pos
  minimizePosition (VersionNotInRange a b c pos) = VersionNotInRange a b c <$> getMinimized pos
  minimizePosition (DisjointRanges a b p1 c p2) = DisjointRanges a b <$> getMinimized p1 <@> c <*> getMinimized p2

  minimized = minimizeSolverPositions index goals collected

  lookupMinimized package versions = map sequence $ versions # traverse
    \version -> Map.lookup (Tuple package version) minimized
  getMinimized (Solving package versions _)
    | Just (First pos) <- lookupMinimized package versions = Solving package versions <$> NEA.nub pos
  getMinimized pos = pure pos

groupErrors :: NonEmptyArray SolverError -> NonEmptyArray SolverError
groupErrors = compose groupErrors2 $ fromGroup <=< NEA.groupAllBy grouping
  where
  grouping (NoVersionsInRange p1 v1 r1 _) (NoVersionsInRange p2 v2 r2 _) =
    compare p1 p2 <> compare v1 v2 <> compare (Range.print r1) (Range.print r2)
  grouping (NoVersionsInRange _ _ _ _) _ = LT
  grouping _ (NoVersionsInRange _ _ _ _) = GT
  grouping (VersionNotInRange p1 v1 r1 _) (VersionNotInRange p2 v2 r2 _) =
    compare p1 p2 <> compare v1 v2 <> compare (Range.print r1) (Range.print r2)
  grouping (VersionNotInRange _ _ _ _) _ = LT
  grouping _ (VersionNotInRange _ _ _ _) = GT
  grouping (DisjointRanges p1 r1 s1 q1 _) (DisjointRanges p2 r2 s2 q2 _) =
    compare p1 p2 <> compare (Range.print r1) (Range.print r2) <> compare s1 s2 <> compare (Range.print q1) (Range.print q2)

  fromGroup es = setPosition (NEA.head es) $ groupPositions $ map getPosition es

  getPosition (NoVersionsInRange _ _ _ p) = p
  getPosition (VersionNotInRange _ _ _ p) = p
  getPosition (DisjointRanges _ _ _ _ p) = p

  setPosition (NoVersionsInRange p v r _) = map $ NoVersionsInRange p v r
  setPosition (VersionNotInRange p v r _) = map $ VersionNotInRange p v r
  setPosition (DisjointRanges p r s q _) = map $ DisjointRanges p r s q

groupErrors2 :: NonEmptyArray SolverError -> NonEmptyArray SolverError
groupErrors2 = map fromGroup <<< NEA.groupAllBy grouping
  where
  grouping (VersionNotInRange p1 _ r1 t1) (VersionNotInRange p2 _ r2 t2) =
    compare p1 p2 <> compare (Range.print r1) (Range.print r2) <> compare t1 t2
  grouping (VersionNotInRange _ _ _ _) _ = LT
  grouping _ (VersionNotInRange _ _ _ _) = GT
  grouping (NoVersionsInRange p1 v1 r1 q1) (NoVersionsInRange p2 v2 r2 q2) =
    compare p1 p2 <> compare v1 v2 <> compare (Range.print r1) (Range.print r2) <> compare q1 q2
  grouping (NoVersionsInRange _ _ _ _) _ = LT
  grouping _ (NoVersionsInRange _ _ _ _) = GT
  grouping (DisjointRanges p1 r1 s1 q1 t1) (DisjointRanges p2 r2 s2 q2 t2) =
    compare p1 p2 <> compare (Range.print r1) (Range.print r2) <> compare s1 s2 <> compare (Range.print q1) (Range.print q2) <> compare t1 t2

  fromGroup :: NonEmptyArray SolverError -> SolverError
  fromGroup es = setVersions (NEA.head es) $ foldMap1 getVersion es

  setVersions (VersionNotInRange p _ r q) (Just vs) = VersionNotInRange p vs r q
  setVersions e _ = e
  getVersion (VersionNotInRange _ vs _ _) = Just vs
  getVersion _ = Nothing

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

printSolverError :: SolverError -> String
printSolverError = case _ of
  NoVersionsInRange name versions range pos -> Array.fold
    [ "Package index contained no versions for "
    , PackageName.print name
    , " in the range "
    , Range.print range
    , " (existing versions: "
    , maybe "none" (intercalateMap ", " Version.print) (NEA.fromFoldable versions)
    , ")"
    , printSolverPosition pos
    ]
  VersionNotInRange name version range pos -> Array.fold
    [ "Committed to "
    , PackageName.print name
    , "@"
    , intercalateMap ", " Version.print version
    , " but the range "
    , Range.print range
    , " was also required"
    , printSolverPosition pos
    ]
  DisjointRanges name range1 pos1 range2 pos2 -> Array.fold
    [ "Committed to "
    , PackageName.print name
    , " in range "
    , Range.print range1
    , printSolverPosition pos1
    , " but the range "
    , Range.print range2
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
      put s
      catchError fb \e2 -> do
        throwError (groupErrors $ e1 <> e2)

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
        Just version | Range.includes range version -> empty
        version -> pure { name, range, version }

solve :: Dependencies -> Map PackageName Range -> Either (NonEmptyArray SolverError) Solved
solve index pending = lmap (minimizeErrors index pending)
  case runRWSE index { pending: map (Tuple SolveRoot) pending, solved: Map.empty } (exploreGoals 20) of
    _ /\ r /\ _ -> r

solveAndValidate :: Dependencies -> Map PackageName Range -> Either (NonEmptyArray SolverError) Solved
solveAndValidate index pending = do
  sols <- solve index pending
  case validate pending sols of
    Left es -> Left $ es <#> \r ->
      case r.version of
        Nothing -> NoVersionsInRange r.name Set.empty r.range SolveRoot
        Just version -> VersionNotInRange r.name (NES.singleton version) r.range SolveRoot
    Right _ -> Right sols

exploreGoals :: Int -> Solver Solved
exploreGoals work =
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
            addVersion pos name version *> exploreGoals 0
        catchError act \e1 -> do
          when (work > 0) $ void do
            put goals'
            catchError (exploreGoals (work - NEA.length e1)) \e2 -> do
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
      if Range.includes newConstraint version then pure unit
      else throwError $ pure $ VersionNotInRange name (NES.singleton version) newConstraint pos

    Nothing ->
      case Map.lookup name pending of
        Nothing -> put $ goals { pending = Map.insert name (Tuple pos newConstraint) pending }

        Just (Tuple oldPos oldConstraint) ->
          case Range.intersect oldConstraint newConstraint of
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
          >>> Array.filter (fst >>> Range.includes constraint)
          >>> NEA.fromArray
  case versions of
    Just vs -> pure vs
    Nothing ->
      throwError $ pure $ NoVersionsInRange name (Map.lookup name index # foldMap Map.keys) constraint pos
