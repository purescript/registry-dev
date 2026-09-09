-- | Compile-guided planning for package set updates.
-- |
-- | The planner probes root selections — packages already in the set plus
-- | proposed additions, each closed over the absent dependencies it requires
-- | — by compiling the whole package set, and repairs failures greedily:
-- |
-- |  1. Probe every root at its latest version.
-- |  2. On failure, parse the failing packages out of the compiler output and
-- |     drop the implicated roots, then retry. Every retry drops at least
-- |     one root, so this terminates.
-- |  3. Retry each dropped root individually against the accepted payload,
-- |     latest version first and then older pending versions round-robin.
-- |     Step 2 may drop more roots than strictly necessary, and a root whose
-- |     latest version is incompatible may still have an intermediate one.
-- |  4. Probe still-blocked roots one interaction component at a time,
-- |     promoting coordinated groups that only compile together.
-- |
-- | The returned `verified` payload is always exactly the change set accepted
-- | by its most recent successful whole-set compile probe, so it is safe to
-- | submit as an atomic package set update. Greedy repair is not guaranteed
-- | to find a maximal payload; whatever it cannot include is reported in
-- | `blocked` with compiler evidence.
-- |
-- | Blocked groups can then be analyzed with `analyzeRemovals`, which
-- | computes the reverse-dependency closure of the unchanged packages the
-- | compiler implicated and verifies — again by an exact compile — that
-- | removing them lets the upgrade through. Compilation keeps exposing
-- | blockers one at a time, so the analysis expands the closure iteratively
-- | until the payload compiles or nothing new is learned.
-- |
-- | Infrastructure failures (storage, git, a missing compiler) surface via
-- | `EXCEPT` and are never conflated with compiler incompatibility.
-- |
-- | Planning is compute-intensive: every probe downloads and compiles an
-- | entire package set. The registry server only verifies and publishes
-- | exact submitted payloads; planning itself runs wherever compute is
-- | available, currently the package set updater and version checker
-- | scripts in CI.
module Registry.App.PackageSetPlanner
  ( BlockedGroup
  , Candidates
  , Plan
  , Probe
  , ProbeResult(..)
  , RemovalAnalysis(..)
  , RemovalReport
  , analyzeRemovals
  , existingPackageCandidates
  , maxPlanProbes
  , planUpgrades
  , probeAtomic
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Registry.App.Effect.PackageSets (PACKAGE_SETS)
import Registry.App.Effect.PackageSets as PackageSets
import Registry.Manifest (Manifest(..))
import Registry.ManifestIndex (ManifestIndex)
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata (Metadata(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.PackageSet (PackageSet(..))
import Registry.Version (Version)
import Run (Run)
import Run.Except (EXCEPT)

-- | Packages eligible for the next package set, each with its pending
-- | versions. The map makes duplicate candidates unrepresentable, every
-- | candidate offers at least one pending version, and the set leaves no room
-- | for duplicate or misordered versions: the maximum is the latest. A
-- | candidate's current version, when it has one, is the version recorded in
-- | the package set itself.
type Candidates = Map PackageName (NonEmptySet Version)

-- | The observable result of compiling one exact proposed package set.
-- | Infrastructure failures are not represented here: a probe raises them
-- | via `EXCEPT`, so they can never be mistaken for incompatibility.
data ProbeResult = Compiles | CompilationFailure String

derive instance Eq ProbeResult

-- | Compile the package set that results from applying the change set to the
-- | given baseline, reporting whether that exact set compiles.
type Probe r = PackageSet -> PackageSets.ChangeSet -> Run r ProbeResult

-- | The standard probe, which builds and compiles the proposed package set.
probeAtomic :: forall r. Probe (PACKAGE_SETS + EXCEPT String + r)
probeAtomic packageSet changes =
  PackageSets.upgradeAtomic packageSet (un PackageSet packageSet).compiler changes <#> case _ of
    Left error -> CompilationFailure error
    Right _ -> Compiles

-- | The result of compile-guided planning. `verified` is exactly the change
-- | set most recently accepted by a successful whole-set compile probe.
-- | Candidates whose latest version could not be included appear in
-- | `blocked`, grouped by dependency interaction.
type Plan =
  { blocked :: Array BlockedGroup
  , probes :: Int
  , truncated :: Boolean
  , verified :: Map PackageName Version
  }

-- | Latest-version updates that could not be included automatically, along
-- | with the compiler evidence from probing them together against the
-- | verified payload. `evidence` is `Nothing` when the probe budget ran out
-- | before the group was probed. `blockers` are the unchanged packages the
-- | compiler implicated.
type BlockedGroup =
  { blockers :: Array PackageName
  , evidence :: Maybe String
  , targets :: Map PackageName Version
  }

-- | The outcome of removal analysis for a blocked group. Only a verified
-- | result carries a submittable payload: `updates` is the exact update map
-- | the successful removal probe compiled — the group's targets together with
-- | the plan's verified upgrades — so a payload that was never compile-
-- | verified cannot be mistaken for one that was.
data RemovalAnalysis
  = RemovalsVerified { removed :: Set PackageName, updates :: Map PackageName Version }
  | RemovalsUnverified (Set PackageName)
  | RemovalsNotAnalyzed String

derive instance Eq RemovalAnalysis

-- | `targets` are the blocked group's latest-version upgrades. `evidence` is
-- | the combined compiler output of every probe involving the group, or
-- | `Nothing` when the group was never probed.
type RemovalReport =
  { analysis :: RemovalAnalysis
  , blockers :: Array PackageName
  , evidence :: Maybe String
  , targets :: Map PackageName Version
  }

-- | Probe budget for automatic planning (phases 1 through 4).
maxPlanProbes :: Int
maxPlanProbes = 60

-- | Overall probe budget, including removal analysis.
maxTotalProbes :: Int
maxTotalProbes = 100

-- | All newer published versions of packages already in the set. Compiler
-- | metadata and manifest ranges are deliberately not eligibility filters:
-- | the compiler decides what is compatible.
existingPackageCandidates :: PackageSet -> Map PackageName Metadata -> Candidates
existingPackageCandidates (PackageSet packageSet) metadata = packageSet.packages # Map.mapMaybeWithKey \name current -> do
  Metadata packageMetadata <- Map.lookup name metadata
  NonEmptySet.fromSet $ Set.filter (_ > current) $ Map.keys packageMetadata.published

-- | The latest version of each candidate.
latestVersions :: Candidates -> Map PackageName Version
latestVersions = map NonEmptySet.max

-- | Plan a compile-verified upgrade. The roots — candidates already in the
-- | package set plus the `seeds` proposed as additions in their own right —
-- | are the packages the planner tries to upgrade. Any other candidate is
-- | support: it enters a payload only through the dependency closure of a
-- | root selection, always at its latest pending version, so a support
-- | package can never be proposed, blocked, or removed on its own.
planUpgrades
  :: forall r
   . Probe (EXCEPT String + r)
  -> PackageSet
  -> ManifestIndex
  -> Set PackageName
  -> Candidates
  -> Run (EXCEPT String + r) Plan
planUpgrades probe packageSet@(PackageSet set) manifests seeds candidates = do
  repaired <- repair (latestVersions rootCandidates) 0
  retried <- if isJust repaired.unattributed then pure repaired else retry repaired
  componentPhase retried
  where
  -- Roots are the independently proposed upgrades; every other candidate is
  -- support and only ever appears inside a root's dependency closure.
  rootCandidates = Map.filterWithKey (\name _ -> Map.member name set.packages || Set.member name seeds) candidates

  -- | The exact payload proposing one root selection: the root itself plus
  -- | every absent package it transitively requires, each at its latest
  -- | pending version. Pinning support to the latest version keeps every
  -- | closure consistent, so unioning closures never conflicts. `Nothing`
  -- | when a required package has no pending version or no manifest, in
  -- | which case no payload containing this selection can ever compile.
  rootClosure :: PackageName -> Version -> Maybe (Map PackageName Version)
  rootClosure rootName rootVersion = go Map.empty [ Tuple rootName rootVersion ]
    where
    go acc frontier = case Array.uncons frontier of
      Nothing -> Just acc
      Just { head: Tuple name version, tail }
        | Map.member name acc -> go acc tail
        | otherwise -> case ManifestIndex.lookup name version manifests of
            Nothing -> Nothing
            Just (Manifest manifest) -> do
              let absent = Set.difference (Map.keys manifest.dependencies) (Map.keys set.packages)
              support <- traverse (\dep -> Tuple dep <<< NonEmptySet.max <$> Map.lookup dep candidates) (Array.fromFoldable absent)
              go (Map.insert name version acc) (tail <> support)

  -- The closure of each root selection. Roots whose closure is impossible
  -- are dropped without spending a probe.
  closeRoots :: Map PackageName Version -> Map PackageName (Map PackageName Version)
  closeRoots = Map.mapMaybeWithKey rootClosure

  payloadOf :: Map PackageName (Map PackageName Version) -> Map PackageName Version
  payloadOf = Map.unions <<< Map.values

  -- Phase 1 and 2: probe all roots at their latest versions (closed over
  -- support) and greedily drop the roots whose closures the compiler
  -- implicates.
  repair rootPayload probes = do
    let closures = closeRoots rootPayload
    let payload = payloadOf closures
    if Map.isEmpty payload then
      pure { verified: Map.empty, probes, truncated: false, unattributed: Nothing }
    else if probes >= maxPlanProbes then
      pure { verified: Map.empty, probes, truncated: true, unattributed: Nothing }
    else
      probe packageSet (map PackageSets.Update payload) >>= case _ of
        Compiles -> pure { verified: payload, probes: probes + 1, truncated: false, unattributed: Nothing }
        CompilationFailure evidence -> do
          let offenders = attributeFailure packageSet manifests payload evidence
          let offendingRoots = Map.keys $ Map.filter (\closure -> not $ Set.isEmpty $ Set.intersection offenders $ Map.keys closure) closures
          if Set.isEmpty offendingRoots then
            pure { verified: Map.empty, probes: probes + 1, truncated: false, unattributed: Just evidence }
          else
            repair (Foldable.foldr Map.delete rootPayload offendingRoots) (probes + 1)

  -- A root is blocked when its latest version is not in the verified
  -- payload, even if an intermediate version was accepted.
  blockedRoots state = Map.filterWithKey (\name versions -> Map.lookup name state.verified /= Just (NonEmptySet.max versions)) rootCandidates

  -- Phase 3: greedy repair may drop more roots than necessary, and a root
  -- blocked at its latest version may still have a compatible intermediate
  -- one. Retry each dropped root individually against the accepted payload,
  -- taking its pending versions newest first and round-robin across roots so
  -- one root with many versions cannot starve the others.
  retry state = go state initialQueue
    where
    -- Pending versions in descending order (latest first).
    initialQueue = Map.toUnfoldable (blockedRoots state) <#> \(Tuple name versions) ->
      Tuple name (Array.reverse $ Array.fromFoldable versions)

    go st queue = case Array.uncons queue of
      Nothing -> pure st
      Just { head: Tuple name versions, tail }
        | st.truncated -> pure st
        | otherwise -> case Array.uncons versions of
            Nothing -> go st tail
            Just { head: version, tail: older }
              | st.probes >= maxPlanProbes -> pure st { truncated = true }
              | otherwise -> case rootClosure name version of
                  Nothing -> go st (tail <> [ Tuple name older ])
                  Just closure -> do
                    let payload = Map.union closure st.verified
                    probe packageSet (map PackageSets.Update payload) >>= case _ of
                      Compiles -> go (st { verified = payload, probes = st.probes + 1 }) tail
                      CompilationFailure _ -> go (st { probes = st.probes + 1 }) (tail <> [ Tuple name older ])

  -- Phase 5: probe the remaining blocked roots one interaction component at
  -- a time. A coordinated group that only compiles together is promoted into
  -- the verified payload; anything else becomes a blocked group carrying the
  -- compiler evidence for removal analysis.
  componentPhase state = do
    let blocked = blockedRoots state
    let components = interactionComponents packageSet manifests blocked
    result <- Array.foldM step (Tuple state []) components
    case result of
      Tuple final groups -> pure
        { blocked: groups
        , probes: final.probes
        , truncated: final.truncated
        , verified: final.verified
        }
    where
    step (Tuple st groups) component = do
      let rootTargets = latestVersions component
      let closures = closeRoots rootTargets
      let unclosable = Set.difference (Map.keys rootTargets) (Map.keys closures)
      if st.probes >= maxPlanProbes || st.truncated then
        pure $ Tuple (st { truncated = true }) (groups <> [ { blockers: [], evidence: Nothing, targets: rootTargets } ])
      else if not (Set.isEmpty unclosable) then do
        let missing = String.joinWith ", " (map PackageName.print (Array.fromFoldable unclosable))
        let evidence = "The proposed payload is not self-contained. These members require a package that is outside the proposed set and has no pending version or manifest: " <> missing
        pure $ Tuple st (groups <> [ { blockers: [], evidence: Just evidence, targets: rootTargets } ])
      else do
        let targets = payloadOf closures
        let payload = Map.union targets st.verified
        probe packageSet (map PackageSets.Update payload) >>= case _ of
          Compiles ->
            pure $ Tuple (st { verified = payload, probes = st.probes + 1 }) groups
          CompilationFailure evidence -> do
            let failing = failingPackages (applyUpdates packageSet payload) evidence
            let blockers = Array.fromFoldable (Set.difference failing (Map.keys payload))
            pure $ Tuple (st { probes = st.probes + 1 }) (groups <> [ { blockers, evidence: Just evidence, targets } ])

-- | Analyze each blocked group of a plan: compute the reverse-dependency
-- | closure of the implicated unchanged packages and verify by compiling that
-- | removing them lets the group's upgrades through. Compilation may expose
-- | further blockers one at a time, so the closure expands iteratively until
-- | the exact payload compiles or nothing new is learned.
analyzeRemovals
  :: forall r
   . Probe (EXCEPT String + r)
  -> PackageSet
  -> ManifestIndex
  -> Plan
  -> Run (EXCEPT String + r) (Array RemovalReport)
analyzeRemovals probe packageSet manifests plan = map snd $ Array.foldM step (Tuple plan.probes []) plan.blocked
  where
  step (Tuple probes reports) group = do
    Tuple next report <- analyzeGroup probes group
    pure $ Tuple next (reports <> [ report ])

  analyzeGroup probes group = case group.evidence of
    Nothing ->
      pure $ Tuple probes $ report implicated [] $ RemovalsNotAnalyzed "The probe budget was exhausted before this group was probed."
    Just evidence
      | Set.isEmpty implicated ->
          pure $ Tuple probes $ report implicated [ evidence ] $ RemovalsNotAnalyzed "The compiler output did not implicate an unchanged package that could be removed."
      | otherwise ->
          expand implicated [ evidence ] probes

    where
    -- The unchanged packages the compiler implicated when the group was probed.
    implicated = Set.fromFoldable group.blockers

    -- The full update map every removal probe compiles: the group's targets
    -- applied on top of the already-verified payload.
    payload = Map.union group.targets plan.verified

    expand blockers evidences used = do
      let proposed = applyUpdates packageSet payload
      case reverseDependencyClosure proposed manifests blockers of
        Left closureError -> pure $ Tuple used (report blockers evidences (RemovalsNotAnalyzed ("Removal analysis failed: " <> closureError)))
        Right removals -> do
          -- If a proposed update transitively depends on a removed package,
          -- no set containing this group can be self-contained after the
          -- removals: the proposal is structurally impossible, not merely
          -- incompatible, so no probe is spent on it.
          let protected = Set.intersection removals (Map.keys payload)
          if not (Set.isEmpty protected) then do
            let names = String.joinWith ", " (map PackageName.print (Array.fromFoldable protected))
            pure $ Tuple used (report blockers evidences (RemovalsNotAnalyzed ("Removing the implicated packages would also require removing proposed upgrades: " <> names <> ".")))
          else if used >= maxTotalProbes then
            pure $ Tuple used (report blockers evidences (RemovalsNotAnalyzed "The probe budget was exhausted before the removal payload could be verified."))
          else do
            let removalChanges = Map.fromFoldable $ map (\name -> Tuple name PackageSets.Remove) (Array.fromFoldable removals :: Array _)
            let changes = Map.union removalChanges (map PackageSets.Update payload)
            probe packageSet changes >>= case _ of
              Compiles -> pure $ Tuple (used + 1) (report blockers evidences (RemovalsVerified { removed: removals, updates: payload }))
              CompilationFailure next -> do
                let failing = failingPackages proposed next
                let exposed = Set.difference (Set.difference failing (Map.keys payload)) blockers
                if Set.isEmpty exposed then
                  pure $ Tuple (used + 1) (report blockers (evidences <> [ next ]) (RemovalsUnverified removals))
                else
                  expand (Set.union blockers exposed) (evidences <> [ next ]) (used + 1)

    report blockers evidences analysis =
      { analysis
      , blockers: Array.fromFoldable blockers
      , evidence: combineCompilerEvidence evidences
      , targets: group.targets
      }

-- | Determine which payload members to drop, given the packages implicated by
-- | compiler output. A failing payload member is dropped directly; a failing
-- | unchanged package implicates the payload members it transitively depends
-- | on, since one of them broke it even though the evidence cannot say which.
attributeFailure :: PackageSet -> ManifestIndex -> Map PackageName Version -> String -> Set PackageName
attributeFailure packageSet manifests payload evidence = Set.union direct fromCones
  where
  applied = applyUpdates packageSet payload
  failing = failingPackages applied evidence
  payloadNames = Map.keys payload
  direct = Set.intersection failing payloadNames
  unchanged = Set.difference failing payloadNames
  cones = Foldable.foldl (\acc name -> Set.union acc (dependencyCone applied manifests name)) Set.empty unchanged
  fromCones = Set.intersection payloadNames cones

-- | The transitive dependency cone of a package, resolved against the exact
-- | applied package versions. Includes the root.
dependencyCone :: Map PackageName Version -> ManifestIndex -> PackageName -> Set PackageName
dependencyCone applied manifests root = go Set.empty [ root ]
  where
  go seen frontier = case Array.uncons frontier of
    Nothing -> seen
    Just { head, tail }
      | Set.member head seen -> go seen tail
      | otherwise -> do
          let
            dependencies = case flip (ManifestIndex.lookup head) manifests =<< Map.lookup head applied of
              Nothing -> []
              Just (Manifest manifest) -> Array.fromFoldable (Map.keys manifest.dependencies)
          go (Set.insert head seen) (tail <> dependencies)

-- | Parse the packages implicated by compiler output, matching the exact
-- | package versions installed for the probe (source paths have the form
-- | packages/<name>@<version>/...).
failingPackages :: Map PackageName Version -> String -> Set PackageName
failingPackages applied evidence = Map.keys (Map.filterWithKey implicated applied)
  where
  fileLines = Array.filter (String.contains (String.Pattern "File:")) (String.split (String.Pattern "\n") evidence)
  implicated name version = do
    let path = "packages/" <> formatPackageVersion name version <> "/"
    any (String.contains (String.Pattern path)) fileLines

-- | Group candidates by dependency-name interaction: two candidates are in
-- | the same component when some relevant manifest mentions both. Components
-- | can be probed independently of one another.
interactionComponents :: PackageSet -> ManifestIndex -> Candidates -> Array Candidates
interactionComponents (PackageSet packageSet) manifests candidates = map candidatesInSet (setsFromGraph graph candidateNames)
  where
  candidateNames = Map.keys candidates
  relevantManifests = Array.catMaybes do
    Tuple name versions <- Map.toUnfoldable candidates
    version <- Array.catMaybes ([ Map.lookup name packageSet.packages ] <> map Just (Array.fromFoldable versions))
    [ ManifestIndex.lookup name version manifests ]
  baselineManifests = Array.mapMaybe (\(Tuple name version) -> ManifestIndex.lookup name version manifests) (Map.toUnfoldable packageSet.packages)
  graph = Foldable.foldl addManifest (Map.fromFoldable $ map (\name -> Tuple name Set.empty) (Array.fromFoldable candidateNames)) (baselineManifests <> relevantManifests)

  addManifest edges (Manifest manifest) = do
    let mentioned = Set.intersection candidateNames (Set.insert manifest.name (Map.keys manifest.dependencies))
    mentioned # Foldable.foldl
      (\next name -> Map.alter (Just <<< Set.union (Set.delete name mentioned) <<< fromMaybe Set.empty) name next)
      edges

  candidatesInSet names = Map.filterKeys (_ `Set.member` names) candidates

setsFromGraph :: Map PackageName (Set PackageName) -> Set PackageName -> Array (Set PackageName)
setsFromGraph graph = go []
  where
  go components remaining = case Set.findMin remaining of
    Nothing -> components
    Just seed -> do
      let component = visit Set.empty [ seed ]
      go (components <> [ component ]) (Set.difference remaining component)

  visit seen frontier = case Array.uncons frontier of
    Nothing -> seen
    Just { head: name, tail: rest }
      | Set.member name seen -> visit seen rest
      | otherwise -> do
          let neighbours = Array.fromFoldable $ fromMaybe Set.empty $ Map.lookup name graph
          visit (Set.insert name seen) (rest <> neighbours)

applyUpdates :: PackageSet -> Map PackageName Version -> Map PackageName Version
applyUpdates (PackageSet packageSet) updates = Map.union updates packageSet.packages

-- | The packages that would have to be removed along with the given packages
-- | to keep the set self-contained: every package that transitively depends
-- | on a removed package must also be removed.
reverseDependencyClosure :: Map PackageName Version -> ManifestIndex -> Set PackageName -> Either String (Set PackageName)
reverseDependencyClosure packages manifests = go
  where
  go removals = do
    dependants <- Set.fromFoldable <<< Array.catMaybes <$> traverse (dependsOn removals) (Map.toUnfoldable packages)
    let next = Set.union removals dependants
    if next == removals then pure removals else go next

  dependsOn removals (Tuple name version) = do
    if Set.member name removals then pure Nothing
    else case ManifestIndex.lookup name version manifests of
      Nothing -> Left $ "Missing manifest while computing removal closure: " <> formatPackageVersion name version
      Just (Manifest manifest) -> pure $ name <$ guard (not $ Set.isEmpty $ Set.intersection removals $ Map.keys manifest.dependencies)

combineCompilerEvidence :: Array String -> Maybe String
combineCompilerEvidence evidences
  | Array.null evidences = Nothing
  | otherwise = Just $ String.joinWith "\n\n--- The next compile probe exposed another blocker ---\n\n" evidences
