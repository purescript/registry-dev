-- | # Public API
module Registry.Solver where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, intercalate)
import Data.FoldableWithIndex (anyWithIndex, foldMapWithIndex, foldlWithIndex, forWithIndex_)
import Data.Functor.App (App(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.List.NonEmpty as NEL
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe, maybe')
import Data.Monoid.Disj (Disj(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (class Newtype, over, un, unwrap, wrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Set (Set)
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NES
import Data.Traversable (for, sequence, traverse)
import Data.TraversableWithIndex (forWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range (Range)
import Registry.Range as Range
import Registry.Version (Version, bumpPatch)
import Registry.Version as Version
import Safe.Coerce (coerce)

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | Data from the registry index, listing dependencies for each version of
-- | each package
type DependencyIndex = Map PackageName (Map Version (Map PackageName Range))

-- | Load a package (asynchronously)
type Loader m = PackageName -> m (Map Version (Map PackageName Range))

-- | Solve a map of requirements given a registry index.
solve :: DependencyIndex -> Map PackageName Range -> Either SolverErrors (Map PackageName Version)
solve index required =
  solveFull
    { registry: initializeRegistry index
    , required: initializeRequired required
    }

loadAndSolve :: forall m. Monad m => Loader m -> Map PackageName Range -> m (Either SolverErrors (Map PackageName Version))
loadAndSolve loader required =
  loadIndex loader required <#> \index ->
    solveFull
      { registry: initializeRegistry index
      , required: initializeRequired required
      }

--------------------------------------------------------------------------------
-- Public API to semi-public API
--------------------------------------------------------------------------------

loadIndex
  :: forall m
   . Monad m
  => Loader m
  -> Map PackageName Range
  -> m DependencyIndex
loadIndex loader required = map _.known <$> go Map.empty (need required)
  where
  need = SemigroupMap <<< map pure
  loadNew package (_ :: Unit) =
    { found: Set.empty, known: _ } <$> loader package

  go
    ::
       -- Packages we have downloaded, and versions whose transitive dependencies
       -- we have already added to `needed` so we never re-scan them
       Map PackageName { known :: Map Version (Map PackageName Range), found :: Set Version }
    ->
    -- Requirements may be disjoint, so we have `Array Range` instead of `Loose`
    SemigroupMap PackageName (Array Range)
    -> m (Map PackageName { known :: Map Version (Map PackageName Range), found :: Set Version })
  go acc (SemigroupMap needed)
    | Just { key: package, value: ranges } <- Map.findMin needed = do
        loaded <- maybe' (loadNew package) pure $ Map.lookup package acc
        let
          needed' = SemigroupMap (Map.delete package needed)
          { needed: neededMore, found: foundMore } = needMore loaded ranges
          loaded' = loaded { found = loaded.found <> foundMore }
          acc' = Map.insert package loaded' acc
        go acc' (needed' <> neededMore)
  go acc _ = pure acc

  needMore
    :: { known :: Map Version (Map PackageName Range), found :: Set Version }
    -> Array Range
    -> { needed :: SemigroupMap PackageName (Array Range), found :: Set Version }
  needMore { known, found } needed =
    let
      isNeeded k = needed # Array.any
        \r -> Range.includes r k
      more = known # Map.filterKeys
        \k -> not Set.member k found && isNeeded k
    in
      { needed: foldMap need more
      , found: found <> Map.keys more
      }

initializeRegistry :: DependencyIndex -> TransitivizedRegistry
initializeRegistry = coerce >>>
  mapWithIndex \package -> mapWithIndex \version ->
    map (intersectionFromRange package version)

initializeRequired :: Map PackageName Range -> SemigroupMap PackageName Intersection
initializeRequired = coerce >>> mapWithIndex intersectionFromRange'

intersectionFromRange :: PackageName -> Version -> Range -> Intersection
intersectionFromRange package version range =
  let
    mkSourced v = Sourced v $ Pos (Solving (NES.singleton { package, version })) Set.empty
  in
    Intersection
      { lower: wrap $ mkSourced (Range.greaterThanOrEq range)
      , upper: wrap $ mkSourced (Range.lessThan range)
      }

intersectionFromRange' :: PackageName -> Range -> Intersection
intersectionFromRange' package range =
  let
    mkSourced v = Sourced v (Pos Root (Set.singleton package))
  in
    Intersection
      { lower: wrap $ mkSourced (Range.greaterThanOrEq range)
      , upper: wrap $ mkSourced (Range.lessThan range)
      }

--------------------------------------------------------------------------------
-- Error types
--------------------------------------------------------------------------------

type SolverErrors = NEL.NonEmptyList SolverError
data SolverError
  = Conflicts (Map PackageName Intersection)
  | WhileSolving PackageName (Map Version SolverError)

derive instance Eq SolverError

--------------------------------------------------------------------------------
-- Error printing
--------------------------------------------------------------------------------

printSolverPosition :: SolverPosition -> String
printSolverPosition = case _ of
  Pos Root _ -> " (declared dependency)"
  Pos Trial _ -> " (attempted version)"
  Pos (Solving local) global ->
    " seen in " <> intercalateMap ", " printPackageVersion local
      <> case NEA.fromFoldable (Set.difference global (Set.map _.package (NES.toSet local))) of
        Nothing -> mempty
        Just as -> " from declared dependencies " <> intercalateMap ", " PackageName.print as

printPackageVersion
  :: { package :: PackageName
     , version :: Version
     }
  -> String
printPackageVersion { package, version } =
  PackageName.print package <> "@" <> Version.print version

printSolverError :: SolverError -> String
printSolverError = printErrorAt ""

printErrorAt :: String -> SolverError -> String
printErrorAt indent = case _ of
  Conflicts conflicts -> intercalate ("\n" <> indent) $
    mapWithIndex (printConflict indent) conflicts
  WhileSolving package versions -> Array.fold
    [ "While solving "
    , PackageName.print package
    , " each version could not be solved:"
    , fold $ versions # mapWithIndex
        \version nested -> Array.fold
          [ "\n"
          , indent
          , "- "
          , Version.print version
          , ": "
          , "\n"
          , indent <> "  "
          , printErrorAt (indent <> "  ") nested
          ]
    ]

printSourced :: forall i. Newtype i Sourced => i -> String
printSourced = unwrap >>> \(Sourced v pos) ->
  Version.print v <> printSolverPosition pos

printConflict :: String -> PackageName -> Intersection -> String
printConflict indent package range | lowerBound range >= upperBound range = Array.fold
  [ "Conflict in version ranges for "
  , PackageName.print package
  , ":"
  , "\n"
  , indent
  , "  >="
  , printSourced (unwrap range).lower
  , "\n"
  , indent
  , "  <"
  , printSourced (unwrap range).upper
  ]
printConflict indent package range = Array.fold
  [ "No versions found in the registry for "
  , PackageName.print package
  , " in range"
  , "\n"
  , indent
  , "  >="
  , printSourced (unwrap range).lower
  , "\n"
  , indent
  , "  <"
  , printSourced (unwrap range).upper
  ]

--------------------------------------------------------------------------------
-- Core algorithm
--------------------------------------------------------------------------------

type TransitivizedRegistry =
  SemigroupMap PackageName (SemigroupMap Version (SemigroupMap PackageName Intersection))

type RR r =
  { registry :: TransitivizedRegistry
  , required :: SemigroupMap PackageName Intersection
  | r
  }

type RRU = RR (updated :: TransitivizedRegistry)
type RRI = RR (inRange :: TransitivizedRegistry)

-- | The spine of the algorithm: initialize and loop until a direct conflict is
-- | found or the packages are solved. Besides the logic in `solveSteps` (which
-- | is meant for avoiding exponential blow-up), it will fall back to making
-- | progress by solving packages in alphabetical order, starting with the
-- | latest version of the alphabetically-smallest package that isn't pinned to
-- | a single version yet.
solveFull :: RR () -> Either SolverErrors (Map PackageName Version)
solveFull = solveAux <<< solveSeed <<< withReachable
  where
  solveAux
    :: RRU -> Either SolverErrors (Map PackageName Version)
  solveAux = solveSteps >>> \r -> do
    -- Memoize computation of `getPackageRange` for `checkRequired` and `checkSolved`
    let rScanned = withInRange r
    -- Check if there are any direct errors in requirements
    lmap pure (checkRequired rScanned)
    -- Check if there is a complete solution, or pick the alphabetically-lowest
    -- package to try versions for to make progress.
    case checkSolved rScanned of
      Right solved -> Right solved
      Left { package, versions } ->
        -- Fully solve each version, starting from the latest
        let
          sols = mapWithIndex (\version deps -> LastSuccess \_ -> solvePackage r package version deps) versions
        in
          case unwrap (sequence sols) unit of
            -- Found a solution, great!
            Right solved -> Right solved
            -- A map of errors from each version
            Left errors ->
              Left $ pure $ WhileSolving package $
                -- Each branch could report multiple errors, but just take one
                NEL.head <$> errors
  applyPackage r package version dependencies =
    let
      required = r.required <> soleVersionOf package version <> dependencies
      updated = maybe Map.empty (Map.singleton package) $ Map.lookup package (unwrap r.registry)
    in
      { required, registry: r.registry, updated: SemigroupMap updated }
  solvePackage r package version dependencies =
    solveAux (trimReachable (applyPackage r package version dependencies))

-- | Check if there are any obvious conflicts affecting the known requirements
-- | by now. These are:
-- | - The lower bound for the requirement is no longer below the upper bound.
-- | - The range is valid, but there are no versions for it in the registry.
-- | - All versions in the registry have some known error in their requirements.
checkRequired :: RRI -> Either SolverError Unit
checkRequired { registry, required, inRange: SemigroupMap inRange } =
  foldlWithIndex (\i b a -> checkRequirement i a b) (Right unit) required
  where
  checkRequirementShallow package range =
    let
      versions = unwrap $ getPackageRange registry package range
    in
      Map.isEmpty versions
  checkRequirement package range previous =
    let
      versions = unwrap $ unwrap <$> fromMaybe mempty (Map.lookup package inRange)
      -- A requirement is invalid if it is not a valid range
      -- or has no versions in the set, but these are the same
      -- check since an invalid range matches no versions ever
      noVersions = Map.isEmpty versions
      -- A requirement may also have errors at all of its versions
      -- which we would love to know before committing to any versions of
      -- other packages!!!!! (to avoid exponential behavior)
      hasErrored = forWithIndex versions \_ deps ->
        -- TODO do this recursively and memoized? nah does not seem necessary
        let
          failedDeps = Map.filterWithKey checkRequirementShallow deps
        in
          if Map.isEmpty failedDeps then Left unit else Right failedDeps
    in
      case noVersions, hasErrored, previous of
        true, _, Left (Conflicts cs) -> Left $ Conflicts $ Map.insert package range cs
        true, _, _ -> Left $ Conflicts $ Map.singleton package range
        false, Right allVersionsFailed, Right _ ->
          Left $ WhileSolving package (Conflicts <$> allVersionsFailed)
        false, _, _ -> previous

-- | Get the latest version of each requirement
getLatest
  :: forall r
   . { inRange :: TransitivizedRegistry
     | r
     }
  -> Maybe (Map PackageName { version :: Version, dependencies :: SemigroupMap PackageName Intersection })
getLatest { inRange: SemigroupMap inRange } =
  for inRange \(SemigroupMap possibilities) -> do
    { key, value } <- Map.findMax possibilities
    pure { version: key, dependencies: value }

-- | Try the latest available versions of each package. This is safe/optimal
-- | because bounds only shrink as required, so if the latest bounds already
-- | satisfy all of the requirements, those bounds won't ever need to shrink and
-- | this is the solution we would find anyways.
tryLatest
  :: forall r
   . { inRange :: TransitivizedRegistry
     | r
     }
  -> Maybe (Map PackageName Version)
tryLatest r = do
  sol <- getLatest r
  -- By construction this satisfies required, so we just
  -- need to check that each has its dependencies included
  for sol \{ version, dependencies } -> do
    forWithIndex_ dependencies \dep range -> do
      { version: vDep } <- Map.lookup dep sol
      guard (satisfies vDep range)
    pure version

-- | See if all packages are solved, or return the alphabetically-smallest
-- | package that has multiple versions left.
checkSolved
  :: forall r
   . { inRange :: TransitivizedRegistry
     | r
     }
  -> Either
       { package :: PackageName, versions :: Map Version (SemigroupMap PackageName Intersection) }
       (Map PackageName Version)
checkSolved r | Just solution <- tryLatest r = pure solution
checkSolved { inRange: SemigroupMap inRange } =
  inRange # traverseWithIndex \package (SemigroupMap possibilities) ->
    case Map.size possibilities, Map.findMax possibilities of
      1, Just { key: version } -> pure version
      _, _ -> Left { package, versions: possibilities }

--------------------------------------------------------------------------------
-- Algorithm for quasi-transitive dependencies
--------------------------------------------------------------------------------

-- | The key to efficiency: take information from the bounds of global
-- | requirements and add it to the local requirements of each package version
-- | in the registry, BUT remove redundant bounds as we do so.
-- |
-- | For example, if we have a global requirement `>=3.1.0 <4.0.0`, then in the
-- | registry we will keep local dependency ranges for the same package that
-- | look like `>=3.2.0 <4.0.0` or `>=3.1.0 <3.9.0` and remove ranges like
-- | `>=3.0.0 <4.0.0` or `>=3.1.0 <4.0.0` itself.
addFrom :: SemigroupMap PackageName Intersection -> SemigroupMap PackageName Intersection -> SemigroupMap PackageName Intersection
addFrom (SemigroupMap required) = over SemigroupMap $ Map.mapMaybeWithKey \package -> case Map.lookup package required of
  Nothing -> Just
  Just i -> \j ->
    if j `wouldUpdate` i then Just (j <> i)
    else Nothing

-- | Used in `addFrom, `wouldUpdate j i` is an optimized version of
-- | `(i <> j /= i)`.
wouldUpdate :: Intersection -> Intersection -> Boolean
wouldUpdate j i = lowerBound j > lowerBound i || upperBound j < upperBound i

-- | We record what dependency ranges are required no matter which version
-- | of the package we pick from the registry. That is, we report the loosest
-- | bounds when all packages report a bound for it. By filling in transitive
-- | dependencies on the registry itself, then, these bounds become more
-- | accurate.
-- |
-- | Also note that removing the redundant requirements via `addFrom` is safe
-- | with the assumptions here: if one local requirement is equal to or looser
-- | than a global requirement, then this result here would also be equal to or
-- | looser than the global requirement.
commonDependencies
  :: TransitivizedRegistry
  -> PackageName
  -> Intersection
  -> SemigroupMap PackageName Intersection
commonDependencies registry package range =
  let
    inRange =
      getPackageRange registry package range
    solvableInRange =
      Array.mapMaybe (traverse toLoose) (Array.fromFoldable inRange)
  in
    case NEA.fromArray solvableInRange of
      Nothing -> mempty
      Just versionDependencies ->
        case NEA.foldMap1 App (un SemigroupMap <$> versionDependencies) of
          App reqs ->
            SemigroupMap $ reqs <#> asDependencyOf range <<< fromLoose

-- | Add quasi transitive dependencies until it stabilizes (no more updates).
-- | Needs to know what was updated since it last ran.
solveSteps :: RRU -> RR ()
solveSteps r0 = go r0
  where
  go r@{ registry, required } | noUpdates r = { registry, required }
  go r = go (solveStep r)

-- | Discover one step of quasi transitive dependencies, for known requirements
-- | and the rest of the registry too.
solveStep :: RRU -> RRU
solveStep initial =
  { required: initial.required <> moreRequired
  , registry: moreRegistry
  , updated: updated <> updatedOfReqs
  }
  where
  -- Transitivize direct requirements
  moreRequired = initial.required # foldMapWithIndex (commonDependencies initial.registry)
  -- Record updates to them
  updatedOfReqs = requirementUpdates initial moreRequired
  -- Transitivize the rest of the registry, which should be:
  --   (1) Pruned at the start to only reachable package versions
  --   (2) Only touching packages that were directly updated last round
  { updated, registry: moreRegistry } = exploreTransitiveDependencies (initial { registry = map (addFrom moreRequired) <$> initial.registry })

-- | Update package versions in the registry with their quasi-transitive
-- | dependencies, if their dependencies were updated in the last tick. The set
-- | global requirements is needed here because those are elided from the
-- | dependencies in each package version, so to tell how the local requirements
-- | updated we need need to peek at that (see `majorUpdate`).
exploreTransitiveDependencies :: RRU -> RRU
exploreTransitiveDependencies lastTick = (\t -> { required: lastTick.required, updated: accumulated (fst t), registry: snd t }) $
  lastTick.registry # traverseWithIndex \package -> traverseWithIndex \version deps ->
    let
      updateOne depName depRange = case Map.isEmpty (unwrap (getPackageRange lastTick.updated depName depRange)) of
        true -> mempty
        false -> Tuple (Disj true) (commonDependencies lastTick.registry depName depRange)
      Tuple (Disj peek) newDeps = foldMapWithIndex updateOne deps
      -- keep GC churn down by re-using old deps if nothing changed, maybe?
      dependencies = if peek then deps <> newDeps else deps
      updated = case peek && majorUpdate lastTick.required deps dependencies of
        true -> doubleton package version dependencies
        false -> mempty
    in
      Tuple updated dependencies

-- | A package may update because its dependencies tightened, but any reverse
-- | dependencies should have already caught that update in this same tick.
-- | So what we look for is either a new transitive dependency picked up (which
-- | the parent will need to incorporate), or newly failing to solve,
-- | both of which may introduce new dependencies for reverse dependencies
-- | through the `commonDependencies` calculation.
majorUpdate :: SemigroupMap PackageName Intersection -> SemigroupMap PackageName Intersection -> SemigroupMap PackageName Intersection -> Boolean
majorUpdate (SemigroupMap required) (SemigroupMap orig) updated =
  let
    minor = { added: false, failedAlready: false, failedNow: false }

    info :: { added :: Boolean, failedNow :: Boolean, failedAlready :: Boolean }
    info = updated # anyWithIndex \package range ->
      case Map.lookup package orig of
        Nothing ->
          case Map.lookup package required of
            Nothing -> minor { added = true }
            Just range' -> minor { added = lowerBound range > lowerBound range' || upperBound range < upperBound range' }
        Just r -> minor { failedAlready = not good r, failedNow = not good range }
  in
    case info of
      { added: true } -> true
      { failedNow: true, failedAlready: false } -> true
      _ -> false

-- | Track updates to `required` (global requirements), simply tagging all known
-- | versions as updated if the required bounds for a package changed.
requirementUpdates
  :: forall r
   . RR r
  -> SemigroupMap PackageName Intersection
  -> TransitivizedRegistry
requirementUpdates { registry: SemigroupMap registry, required: SemigroupMap required } =
  foldMapWithIndex \package newRange ->
    let
      changed =
        case Map.lookup package required of
          -- This package had a dependency added
          Nothing -> true
          -- The lower or upper bounds tightened
          Just oldRange ->
            lowerBound oldRange < lowerBound newRange ||
              upperBound oldRange > upperBound newRange
    in
      if not changed then mempty
      else
        -- Mark all versions in the registry as updated, just to be safe
        -- (I think technically we would only need to mark versions
        -- that are not included in the new version range …)
        case Map.lookup package registry of
          Just versions -> SemigroupMap $ Map.singleton package versions
          Nothing -> mempty

-- | Trim the registry down to only package versions that are reachable from
-- | the initial set of requirements. Only done once.
gatherReachable :: forall r. RR r -> TransitivizedRegistry
gatherReachable { registry, required } =
  let
    reachable0 :: SemigroupMap PackageName (SemigroupMap Version (SemigroupMap PackageName Intersection))
    reachable0 = mapWithIndex (getPackageRange registry) required
    moreReachable = (foldMap <<< foldMap) (mapWithIndex (getPackageRange registry))
    reachable = fixEqM moreReachable reachable0
  in
    reachable

-- | Also helps with efficiency: remove package versions from the registry
-- | that are outside of the global requirements. Done regularly.
trimReachable :: forall r. RR r -> RR r
trimReachable r = r
  { registry = r.registry # mapWithIndex \package ->
      over SemigroupMap $ Map.filterWithKey \version _ ->
        case Map.lookup package $ unwrap r.required of
          Nothing -> true
          Just range -> satisfies version range
  }

--------------------------------------------------------------------------------
-- Core types
--------------------------------------------------------------------------------

data LocalSolverPosition
  -- | Dependency asked for in manifest
  = Root
  -- | Committed to a specific version
  | Trial
  -- | Required transitive dependency seen in said packages
  | Solving
      ( NonEmptySet
          { package :: PackageName
          , version :: Version
          }
      )

derive instance Eq LocalSolverPosition

instance Semigroup LocalSolverPosition where
  append Trial _ = Trial
  append _ Trial = Trial
  append Root _ = Root
  append _ Root = Root
  append (Solving r1) (Solving r2) = Solving (r1 <> r2)

data SolverPosition = Pos LocalSolverPosition (Set PackageName)

derive instance Eq SolverPosition

instance Semigroup SolverPosition where
  append (Pos l1 g1) (Pos l2 g2) =
    Pos (l1 <> l2) (g1 <> g2)

dependency :: SolverPosition -> SolverPosition -> SolverPosition
dependency (Pos _ g1) (Pos l2 g2) = Pos l2 (g1 <> g2)

dependencyOf :: forall z. Newtype z Sourced => SolverPosition -> z -> z
dependencyOf p1 = coerce \(Sourced v p2) ->
  Sourced v (dependency p1 p2)

asDependencyOf :: Intersection -> Intersection -> Intersection
asDependencyOf (Intersection i1) (Intersection i2) =
  let
    pos = getPos i1.lower <> getPos i1.upper
  in
    Intersection
      { lower: dependencyOf pos i2.lower
      , upper: dependencyOf pos i2.upper
      }

data Sourced = Sourced Version SolverPosition

derive instance Eq Sourced

unSource :: Sourced -> Version
unSource (Sourced v _) = v

getPos :: forall z. Newtype z Sourced => z -> SolverPosition
getPos = unwrap >>> \(Sourced _ pos) -> pos

newtype MinSourced = MinSourced Sourced

derive instance Newtype MinSourced _
derive newtype instance Eq MinSourced
instance Semigroup MinSourced where
  append a@(MinSourced (Sourced av as)) b@(MinSourced (Sourced bv bs)) =
    case compare av bv of
      LT -> a
      GT -> b
      EQ -> MinSourced (Sourced av (as <> bs))

newtype MaxSourced = MaxSourced Sourced

derive instance Newtype MaxSourced _
derive newtype instance Eq MaxSourced
instance Semigroup MaxSourced where
  append a@(MaxSourced (Sourced av as)) b@(MaxSourced (Sourced bv bs)) =
    case compare av bv of
      GT -> a
      LT -> b
      EQ -> MaxSourced (Sourced av (as <> bs))

newtype Intersection = Intersection
  { lower :: MaxSourced
  , upper :: MinSourced
  }

derive instance Newtype Intersection _
derive newtype instance Eq Intersection
derive newtype instance Semigroup Intersection

upperBound :: Intersection -> Version
upperBound (Intersection { upper: MinSourced (Sourced v _) }) = v

lowerBound :: Intersection -> Version
lowerBound (Intersection { lower: MaxSourced (Sourced v _) }) = v

good :: Intersection -> Boolean
good i = upperBound i > lowerBound i

satisfies
  :: Version -> Intersection -> Boolean
satisfies v r = v >= lowerBound r && v < upperBound r

soleVersion :: Version -> Intersection
soleVersion v = Intersection
  { lower: MaxSourced (Sourced v (Pos Trial Set.empty))
  , upper: MinSourced (Sourced (bumpPatch v) (Pos Trial Set.empty))
  }

soleVersionOf :: PackageName -> Version -> SemigroupMap PackageName Intersection
soleVersionOf package v = SemigroupMap (Map.singleton package (soleVersion v))

-- | Filter out the versions of the package that are applicable.
getPackageRange
  :: forall d
   . SemigroupMap PackageName (SemigroupMap Version d)
  -> PackageName
  -> Intersection
  -> SemigroupMap Version d
getPackageRange (SemigroupMap registry) package range =
  case Map.lookup package registry of
    Nothing -> SemigroupMap Map.empty
    Just (SemigroupMap versions) ->
      SemigroupMap $ Map.filterKeys (\v -> v `satisfies` range) versions

newtype Loose = Loose
  { lower :: MinSourced
  , upper :: MaxSourced
  }

derive instance Newtype Loose _

derive newtype instance Semigroup Loose

toLoose :: Intersection -> Maybe Loose
toLoose r | lowerBound r < upperBound r = Just (coerce r)
toLoose _ = Nothing

fromLoose :: Loose -> Intersection
fromLoose = coerce

--------------------------------------------------------------------------------
-- Data management
--------------------------------------------------------------------------------

withReachable :: forall r. RR r -> RR r
withReachable r = r { registry = map (addFrom r.required) <$> gatherReachable r }

withInRange :: RR () -> RRI
withInRange r =
  { registry: r.registry
  , required: r.required
  , inRange: mapWithIndex (getPackageRange r.registry) r.required
  }

solveSeed :: RR () -> RRU
solveSeed { registry, required } = { registry, required, updated: registry }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Lazy swapped Either Applicative
newtype LastSuccess b a = LastSuccess (Unit -> Either a b)

derive instance Newtype (LastSuccess b a) _
instance Functor (LastSuccess b) where
  map f = over LastSuccess (map (lmap f))

instance Apply (LastSuccess b) where
  apply (LastSuccess mf) (LastSuccess ma) = LastSuccess \u ->
    case ma u of
      Right v -> Right v
      Left a ->
        case mf u of
          Right v -> Right v
          Left f -> Left (f a)

instance Applicative (LastSuccess b) where
  pure = LastSuccess <<< pure <<< Left

-- | More efficient SemigroupMap for single updates …
type Acc = Endo (->) TransitivizedRegistry

doubleton :: PackageName -> Version -> SemigroupMap PackageName Intersection -> Acc
doubleton package version dat = coerce $ Map.alter (Just <<< helper) package
  where
  helper Nothing = Map.singleton version dat
  helper (Just v) = Map.insert version dat v

accumulated :: forall a. Monoid a => Endo (->) a -> a
accumulated (Endo f) = f mempty

fixEq :: forall a. Eq a => (a -> a) -> (a -> a)
fixEq f a = let b = f a in if b == a then a else fixEq f b

-- | An optimized fixpoint for semilattice closure operations, accumulating a
-- | full result while only running the function on the newly added bit.
-- |
-- | Invariant: f (acc <> lastAdded) = acc <> f lastAdded
fixEqM :: forall a. Semigroup a => Eq a => (a -> a) -> (a -> a)
fixEqM f = join go
  where
  go acc lastAdded =
    let
      moreAdded = f lastAdded
      moreAcc = acc <> moreAdded
    in
      if moreAcc == acc then acc
      else
        go moreAcc moreAdded

noUpdates :: forall r k v. { updated :: SemigroupMap k v | r } -> Boolean
noUpdates { updated: SemigroupMap updated } = Map.isEmpty updated

exploreAllTransitiveDependencies :: TransitivizedRegistry -> TransitivizedRegistry
exploreAllTransitiveDependencies registry = go { registry, updated: registry, required: mempty }
  where
  go r | noUpdates r = r.registry
  go r = go (exploreTransitiveDependencies r)
