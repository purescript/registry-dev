module Test.Registry.App.PackageSetPlanner (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Registry.App.Effect.PackageSets (Change(..), ChangeSet)
import Registry.App.PackageSetPlanner as Planner
import Registry.Manifest (Manifest)
import Registry.ManifestIndex (IncludeRanges(..), ManifestIndex)
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.PackageSet (PackageSet)
import Registry.PackageSet as PackageSet
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Registry.Version (Version)
import Registry.Version as Version
import Run (Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "compile-guided planning" do
    Spec.it "keeps every newer existing-package version without compiler-history gating" do
      let
        packageSet = mkPackageSet $ Map.singleton (name "example") (version "1.0.0")
        metadata = Map.fromFoldable
          [ Utils.unsafeMetadata "example"
              [ Tuple "1.0.0" [ "0.15.10" ]
              , Tuple "2.0.0" [ "0.14.0" ]
              , Tuple "3.0.0" [ "0.13.0" ]
              ]
          ]
        candidates = Planner.existingPackageCandidates packageSet metadata

      candidates `Assert.shouldEqual` Map.fromFoldable [ candidate "example" [ "3.0.0", "2.0.0" ] ]

    Spec.it "verifies all candidates at their latest versions in a single probe" do
      let
        packageSet = mkPackageSet $ Map.fromFoldable
          [ Tuple (name "a") (version "1.0.0")
          , Tuple (name "b") (version "1.0.0")
          ]
        manifests = manifestIndex
          [ Utils.unsafeManifest "a" "2.0.0" []
          , Utils.unsafeManifest "b" "2.0.0" []
          ]
        candidates = Map.fromFoldable
          [ candidate "a" [ "2.0.0" ]
          , candidate "b" [ "2.0.0" ]
          ]
        plan = plan' (\_ _ -> pure Planner.Compiles) packageSet manifests candidates

      map _.verified plan `Assert.shouldEqual` Right (versionMap [ Tuple "a" "2.0.0", Tuple "b" "2.0.0" ])
      map _.blocked plan `Assert.shouldEqual` Right []
      map _.probes plan `Assert.shouldEqual` Right 1

    Spec.it "treats declared ranges as advisory when the exact set compiles" do
      let
        packageSet = mkPackageSet $ Map.fromFoldable
          [ Tuple (name "dependency") (version "1.0.0")
          , Tuple (name "legacy") (version "1.0.0")
          ]
        manifests = manifestIndex
          [ Utils.unsafeManifest "dependency" "1.0.0" []
          , Utils.unsafeManifest "dependency" "2.0.0" []
          , Utils.unsafeManifest "legacy" "1.0.0" [ Tuple "dependency" ">=3.0.0 <4.0.0" ]
          ]
        candidates = Map.fromFoldable
          [ candidate "dependency" [ "2.0.0" ] ]
        plan = plan' (\_ _ -> pure Planner.Compiles) packageSet manifests candidates

      map _.verified plan `Assert.shouldEqual` Right (versionMap [ Tuple "dependency" "2.0.0" ])

    Spec.it "attributes failures to the exact selected versions and rescues the rest" do
      let
        packageSet = mkPackageSet $ Map.fromFoldable
          [ Tuple (name "a") (version "1.0.0")
          , Tuple (name "b") (version "1.0.0")
          ]
        manifests = manifestIndex
          [ Utils.unsafeManifest "a" "2.0.0" []
          , Utils.unsafeManifest "b" "2.0.0" []
          ]
        candidates = Map.fromFoldable
          [ candidate "a" [ "2.0.0" ]
          , candidate "b" [ "2.0.0" ]
          ]
        -- The evidence names the selected version a@2.0.0, not the baseline
        -- version a@1.0.0, so attribution must resolve against the payload.
        probe _ changes =
          if Map.lookup (name "a") changes == Just (Update (version "2.0.0")) then
            pure $ Planner.CompilationFailure (fileError "a" "2.0.0")
          else pure Planner.Compiles
        plan = plan' probe packageSet manifests candidates

      map _.verified plan `Assert.shouldEqual` Right (versionMap [ Tuple "b" "2.0.0" ])
      map (map _.targets <<< _.blocked) plan `Assert.shouldEqual` Right [ versionMap [ Tuple "a" "2.0.0" ] ]

    Spec.it "implicates payload members through the dependency cone of a failing unchanged package" do
      let
        packageSet = mkPackageSet $ Map.fromFoldable
          [ Tuple (name "lib") (version "1.0.0")
          , Tuple (name "consumer") (version "1.0.0")
          , Tuple (name "other") (version "1.0.0")
          ]
        manifests = manifestIndex
          [ Utils.unsafeManifest "lib" "1.0.0" []
          , Utils.unsafeManifest "lib" "2.0.0" []
          , Utils.unsafeManifest "consumer" "1.0.0" [ Tuple "lib" ">=1.0.0 <3.0.0" ]
          , Utils.unsafeManifest "other" "1.0.0" []
          , Utils.unsafeManifest "other" "2.0.0" []
          ]
        candidates = Map.fromFoldable
          [ candidate "lib" [ "2.0.0" ]
          , candidate "other" [ "2.0.0" ]
          ]
        -- The compiler implicates the unchanged `consumer`, so the planner
        -- must drop the payload member `consumer` depends on: `lib`.
        probe _ changes =
          if Map.lookup (name "lib") changes == Just (Update (version "2.0.0")) then
            pure $ Planner.CompilationFailure (fileError "consumer" "1.0.0")
          else pure Planner.Compiles
        plan = plan' probe packageSet manifests candidates

      map _.verified plan `Assert.shouldEqual` Right (versionMap [ Tuple "other" "2.0.0" ])
      map (map _.targets <<< _.blocked) plan `Assert.shouldEqual` Right [ versionMap [ Tuple "lib" "2.0.0" ] ]

    Spec.it "fails closed when compiler evidence implicates no candidate" do
      let
        packageSet = mkPackageSet $ Map.singleton (name "example") (version "1.0.0")
        manifests = manifestIndex [ Utils.unsafeManifest "example" "2.0.0" [] ]
        candidates = Map.fromFoldable
          [ candidate "example" [ "2.0.0" ] ]
        plan = plan' (\_ _ -> pure $ Planner.CompilationFailure "mysterious failure without file paths") packageSet manifests candidates

      -- Incompatibility is a result, not an error: the plan is empty but Right.
      map _.verified plan `Assert.shouldEqual` Right Map.empty
      map (map _.blockers <<< _.blocked) plan `Assert.shouldEqual` Right [ [] ]

    Spec.it "falls back to a compatible intermediate version when the latest fails" do
      let
        packageSet = mkPackageSet $ Map.singleton (name "example") (version "1.0.0")
        manifests = manifestIndex
          [ Utils.unsafeManifest "example" "2.0.0" []
          , Utils.unsafeManifest "example" "3.0.0" []
          ]
        candidates = Map.fromFoldable
          [ candidate "example" [ "3.0.0", "2.0.0" ] ]
        probe _ changes =
          if Map.lookup (name "example") changes == Just (Update (version "3.0.0")) then
            pure $ Planner.CompilationFailure (fileError "example" "3.0.0")
          else pure Planner.Compiles
        plan = plan' probe packageSet manifests candidates

      map _.verified plan `Assert.shouldEqual` Right (versionMap [ Tuple "example" "2.0.0" ])
      -- The latest version remains reported as blocked.
      map (map _.targets <<< _.blocked) plan `Assert.shouldEqual` Right [ versionMap [ Tuple "example" "3.0.0" ] ]

    Spec.it "promotes a coordinated group that only compiles together" do
      let
        packageSet = mkPackageSet $ Map.fromFoldable
          [ Tuple (name "x") (version "1.0.0")
          , Tuple (name "y") (version "1.0.0")
          , Tuple (name "z") (version "1.0.0")
          ]
        manifests = manifestIndex
          [ Utils.unsafeManifest "x" "2.0.0" []
          , Utils.unsafeManifest "y" "2.0.0" [ Tuple "x" ">=2.0.0 <3.0.0" ]
          , Utils.unsafeManifest "z" "2.0.0" []
          ]
        candidates = Map.fromFoldable
          [ candidate "x" [ "2.0.0" ]
          , candidate "y" [ "2.0.0" ]
          , candidate "z" [ "2.0.0" ]
          ]
        -- `z` fails with evidence that sweeps up `x` and `y`, and `x` and `y`
        -- fail individually, so only probing their interaction component
        -- together can recover the coordinated upgrade.
        probe _ changes = do
          let updated package = Map.lookup (name package) changes == Just (Update (version "2.0.0"))
          if updated "z" then
            pure $ Planner.CompilationFailure $ String.joinWith "\n" [ fileError "z" "2.0.0", fileError "x" "2.0.0", fileError "y" "2.0.0" ]
          else if updated "x" /= updated "y" then
            pure $ Planner.CompilationFailure (fileError (if updated "x" then "x" else "y") "2.0.0")
          else pure Planner.Compiles
        plan = plan' probe packageSet manifests candidates

      map _.verified plan `Assert.shouldEqual` Right (versionMap [ Tuple "x" "2.0.0", Tuple "y" "2.0.0" ])
      map (map _.targets <<< _.blocked) plan `Assert.shouldEqual` Right [ versionMap [ Tuple "z" "2.0.0" ] ]

    Spec.it "drops payload members whose dependencies are outside the proposed set without probing" do
      let
        packageSet = mkPackageSet $ Map.singleton (name "parent") (version "1.0.0")
        -- `missing` has a manifest in the index, but is neither in the
        -- package set nor proposed by the payload.
        manifests = manifestIndex
          [ Utils.unsafeManifest "parent" "2.0.0" [ Tuple "missing" ">=1.0.0 <2.0.0" ]
          , Utils.unsafeManifest "missing" "1.0.0" []
          ]
        candidates = Map.fromFoldable
          [ candidate "parent" [ "2.0.0" ] ]
        plan = plan' (\_ _ -> pure Planner.Compiles) packageSet manifests candidates

      map _.verified plan `Assert.shouldEqual` Right Map.empty
      map _.probes plan `Assert.shouldEqual` Right 0
      case map (map _.evidence <<< _.blocked) plan of
        Right [ Just evidence ] -> String.contains (String.Pattern "outside the proposed set") evidence `Assert.shouldEqual` true
        _ -> Assert.fail "Expected one unclosed blocked group with evidence."

    Spec.it "truncates the search when the probe budget runs out" do
      let
        packageSet = mkPackageSet $ Map.singleton (name "example") (version "1.0.0")
        versions = map (\major -> version (show major <> ".0.0")) (Array.range 2 100)
        manifests = manifestIndex $ map (\v -> Utils.unsafeManifest "example" (Version.print v) []) versions
        candidates = Map.singleton (name "example") (versionSet versions)
        probe _ changes = case Map.lookup (name "example") changes of
          Just (Update selected) -> pure $ Planner.CompilationFailure (fileError "example" (Version.print selected))
          _ -> pure Planner.Compiles
        plan = plan' probe packageSet manifests candidates

      map _.truncated plan `Assert.shouldEqual` Right true
      map _.verified plan `Assert.shouldEqual` Right Map.empty

    Spec.it "keeps a support addition only when a selected version requires it" do
      let
        -- `extra` is a support candidate: it is eligible only because the
        -- fallback root@2.0.0 depends on it. The latest root@3.0.0 does not.
        packageSet = mkPackageSet $ Map.singleton (name "root") (version "1.0.0")
        manifests = manifestIndex
          [ Utils.unsafeManifest "root" "2.0.0" [ Tuple "extra" ">=1.0.0 <2.0.0" ]
          , Utils.unsafeManifest "root" "3.0.0" []
          , Utils.unsafeManifest "extra" "1.0.0" []
          ]
        candidates = Map.fromFoldable
          [ candidate "root" [ "3.0.0", "2.0.0" ]
          , candidate "extra" [ "1.0.0" ]
          ]
        planWith probe = Run.extract $ Except.runExcept $ Planner.planUpgrades probe packageSet manifests Set.empty candidates

      -- When the latest root compiles it does not need `extra`, so the
      -- closure never proposes the addition and the verified payload is
      -- exactly what was compiled.
      let latestPlan = planWith (\_ _ -> pure Planner.Compiles)
      map _.verified latestPlan `Assert.shouldEqual` Right (versionMap [ Tuple "root" "3.0.0" ])

      -- When the planner falls back to root@2.0.0, its `extra` dependency is
      -- genuinely required and stays in the payload.
      let
        fallbackProbe _ changes =
          if Map.lookup (name "root") changes == Just (Update (version "3.0.0")) then
            pure $ Planner.CompilationFailure (fileError "root" "3.0.0")
          else pure Planner.Compiles
      map _.verified (planWith fallbackProbe) `Assert.shouldEqual`
        Right (versionMap [ Tuple "root" "2.0.0", Tuple "extra" "1.0.0" ])

    Spec.it "never blocks or analyzes a support candidate no selected root requires" do
      let
        -- `sup` is eligible only because the fallback root@2.0.0 requires it,
        -- and adding it breaks the set. Since the selected root@3.0.0 does
        -- not need it, `sup` must never be probed, blocked, or recommended.
        packageSet = mkPackageSet $ Map.singleton (name "root") (version "1.0.0")
        manifests = manifestIndex
          [ Utils.unsafeManifest "root" "2.0.0" [ Tuple "sup" ">=1.0.0 <2.0.0" ]
          , Utils.unsafeManifest "root" "3.0.0" []
          , Utils.unsafeManifest "sup" "1.0.0" []
          ]
        candidates = Map.fromFoldable
          [ candidate "root" [ "3.0.0", "2.0.0" ]
          , candidate "sup" [ "1.0.0" ]
          ]
        probe _ changes =
          if Map.member (name "sup") changes then
            pure $ Planner.CompilationFailure (fileError "sup" "1.0.0")
          else pure Planner.Compiles
        plan = Run.extract $ Except.runExcept $ Planner.planUpgrades probe packageSet manifests Set.empty candidates

      map _.verified plan `Assert.shouldEqual` Right (versionMap [ Tuple "root" "3.0.0" ])
      map _.blocked plan `Assert.shouldEqual` Right []
      map _.probes plan `Assert.shouldEqual` Right 1

    Spec.it "does not propose removals that would also remove proposed upgrades" do
      let
        -- `target`'s upgrade transitively depends on the very package the
        -- compiler implicates, so removing the blocker can never yield a
        -- self-contained set that still contains the upgrade.
        packageSet = mkPackageSet $ Map.fromFoldable
          [ Tuple (name "target") (version "1.0.0")
          , Tuple (name "blocker") (version "1.0.0")
          ]
        manifests = manifestIndex
          [ Utils.unsafeManifest "target" "1.0.0" []
          , Utils.unsafeManifest "target" "2.0.0" [ Tuple "blocker" ">=1.0.0 <2.0.0" ]
          , Utils.unsafeManifest "blocker" "1.0.0" []
          ]
        candidates = Map.fromFoldable
          [ candidate "target" [ "2.0.0" ] ]
        probe _ changes =
          if Map.lookup (name "target") changes == Just (Update (version "2.0.0")) then
            pure $ Planner.CompilationFailure (fileError "blocker" "1.0.0")
          else pure Planner.Compiles
        results = analyze' probe packageSet manifests candidates

      case results of
        Right [ report ] -> case report.analysis of
          Planner.RemovalsNotAnalyzed reason ->
            String.contains (String.Pattern "target") reason `Assert.shouldEqual` true
          Planner.RemovalsVerified _ -> Assert.fail "Expected RemovalsNotAnalyzed, got RemovalsVerified"
          Planner.RemovalsUnverified _ -> Assert.fail "Expected RemovalsNotAnalyzed, got RemovalsUnverified"
        other -> Assert.fail $ "Expected exactly one removal report, got: " <> show (map Array.length other)

    Spec.it "does not turn infrastructure failures into compatibility results" do
      let
        packageSet = mkPackageSet $ Map.singleton (name "example") (version "1.0.0")
        manifests = manifestIndex [ Utils.unsafeManifest "example" "2.0.0" [] ]
        candidates = Map.fromFoldable
          [ candidate "example" [ "2.0.0" ] ]
        plan = plan' (\_ _ -> Except.throw "storage unavailable") packageSet manifests candidates

      plan `Assert.shouldEqual` Left "storage unavailable"

    Spec.it "extends a removal proposal when compilation reveals another blocker" do
      let
        packageSet = mkPackageSet $ Map.fromFoldable
          [ Tuple (name "target") (version "1.0.0")
          , Tuple (name "blocked-a") (version "1.0.0")
          , Tuple (name "blocked-b") (version "1.0.0")
          , Tuple (name "unrelated") (version "1.0.0")
          ]
        manifests = manifestIndex
          [ Utils.unsafeManifest "target" "1.0.0" []
          , Utils.unsafeManifest "target" "2.0.0" []
          , Utils.unsafeManifest "blocked-a" "1.0.0" []
          , Utils.unsafeManifest "blocked-b" "1.0.0" []
          , Utils.unsafeManifest "unrelated" "1.0.0" []
          , Utils.unsafeManifest "unrelated" "2.0.0" []
          ]
        candidates = Map.fromFoldable
          [ candidate "target" [ "2.0.0" ]
          , candidate "unrelated" [ "2.0.0" ]
          ]
        probe _ changes
          | Map.lookup (name "target") changes /= Just (Update (version "2.0.0")) = pure Planner.Compiles
          | Map.lookup (name "blocked-a") changes /= Just Remove = pure $ Planner.CompilationFailure (fileError "blocked-a" "1.0.0")
          | Map.lookup (name "blocked-b") changes /= Just Remove = pure $ Planner.CompilationFailure (fileError "blocked-b" "1.0.0")
          | otherwise = pure Planner.Compiles
        results = analyze' probe packageSet manifests candidates

      case results of
        Right [ report ] -> do
          -- The removal probe compiled the blocked target together with the
          -- verified `unrelated` upgrade, so both must appear in the exact
          -- update map recommended for submission.
          report.analysis `Assert.shouldEqual` Planner.RemovalsVerified
            { removed: Set.fromFoldable [ name "blocked-a", name "blocked-b" ]
            , updates: versionMap [ Tuple "target" "2.0.0", Tuple "unrelated" "2.0.0" ]
            }
          report.targets `Assert.shouldEqual` versionMap [ Tuple "target" "2.0.0" ]
          map (String.contains (String.Pattern "blocked-b@1.0.0")) report.evidence `Assert.shouldEqual` Just true
        other -> Assert.fail $ "Expected exactly one removal report, got: " <> show (map Array.length other)

    Spec.it "reserves the removal-analysis budget when probing blocked components" do
      let
        letters = String.split (String.Pattern "") "abcdefgh"
        pkgNames = Array.take 50 do
          a <- letters
          b <- letters
          pure ("pkg-" <> a <> b)
        packageSet = mkPackageSet $ Map.fromFoldable $ map (\pkg -> Tuple (name pkg) (version "1.0.0")) pkgNames
        manifests = manifestIndex $ map (\pkg -> Utils.unsafeManifest pkg "2.0.0" []) pkgNames
        candidates = Map.fromFoldable $ map (\pkg -> candidate pkg [ "2.0.0" ]) pkgNames
        -- Every probe fails with evidence naming every updated package, so
        -- the all-at-once probe drops everything, individual rescues fail,
        -- and the component phase faces one component per candidate: more
        -- than the planning budget allows.
        probe _ changes = do
          let
            errors = Map.toUnfoldable changes # Array.mapMaybe \(Tuple pkg change) -> case change of
              Update selected -> Just (fileError (PackageName.print pkg) (Version.print selected))
              _ -> Nothing
          pure $ Planner.CompilationFailure (String.joinWith "\n" errors)
        plan = plan' probe packageSet manifests candidates

      -- Phases 1 through 5 stop at the planning budget, leaving the rest of
      -- the total budget in reserve for removal analysis.
      map _.probes plan `Assert.shouldEqual` Right Planner.maxPlanProbes
      map _.truncated plan `Assert.shouldEqual` Right true
      map (Array.length <<< _.blocked) plan `Assert.shouldEqual` Right 50

  Spec.describe "the Elmish package-set-79 scenario" do
    let fixture = elmishFixture
    let plan = plan' elmishOracle fixture.packageSet fixture.manifests fixture.candidates
    let removals = analyze' elmishOracle fixture.packageSet fixture.manifests fixture.candidates

    Spec.it "automatically finds the intermediate elmish-html upgrade" do
      map _.verified plan `Assert.shouldEqual` Right (versionMap [ Tuple "elmish-html" "0.11.1" ])

    Spec.it "recognizes the coordinated elmish + elmish-html upgrade blocked by elmish-hooks" do
      map (map _.targets <<< _.blocked) plan `Assert.shouldEqual`
        Right [ versionMap [ Tuple "elmish" "0.14.0", Tuple "elmish-html" "0.12.0" ] ]
      map (map _.blockers <<< _.blocked) plan `Assert.shouldEqual` Right [ [ name "elmish-hooks" ] ]

    Spec.it "verifies the exact removal closure without removing unaffected packages" do
      case removals of
        Right [ report ] -> do
          -- The exact compiled update map: the blocked coordinated targets
          -- override the intermediate elmish-html upgrade from the verified
          -- payload. Unaffected packages like elmish-enzyme must not appear
          -- in the removals.
          report.analysis `Assert.shouldEqual` Planner.RemovalsVerified
            { removed: Set.fromFoldable [ name "elmish-hooks", name "elmish-time-machine" ]
            , updates: versionMap [ Tuple "elmish" "0.14.0", Tuple "elmish-html" "0.12.0" ]
            }
          map (String.contains (String.Pattern "elmish-hooks@0.11.0")) report.evidence `Assert.shouldEqual` Just true
        other -> Assert.fail $ "Expected exactly one removal report, got: " <> show (map Array.length other)

-- | Run the planner with a mock probe, treating every candidate as a
-- | deliberately proposed root.
plan'
  :: (PackageSet -> ChangeSet -> Run (EXCEPT String + ()) Planner.ProbeResult)
  -> PackageSet
  -> ManifestIndex
  -> Planner.Candidates
  -> Either String Planner.Plan
plan' probe packageSet manifests candidates =
  Run.extract $ Except.runExcept $ Planner.planUpgrades probe packageSet manifests (Map.keys candidates) candidates

-- | Run the planner and removal analysis with a mock probe.
analyze'
  :: (PackageSet -> ChangeSet -> Run (EXCEPT String + ()) Planner.ProbeResult)
  -> PackageSet
  -> ManifestIndex
  -> Planner.Candidates
  -> Either String (Array Planner.RemovalReport)
analyze' probe packageSet manifests candidates =
  Run.extract $ Except.runExcept do
    plan <- Planner.planUpgrades probe packageSet manifests (Map.keys candidates) candidates
    Planner.analyzeRemovals probe packageSet manifests plan

type ElmishFixture =
  { candidates :: Planner.Candidates
  , manifests :: ManifestIndex
  , packageSet :: PackageSet
  }

elmishFixture :: ElmishFixture
elmishFixture = do
  let
    packages = Map.fromFoldable
      [ Tuple (name "elmish") (version "0.13.0")
      , Tuple (name "elmish-enzyme") (version "0.1.1")
      , Tuple (name "elmish-hooks") (version "0.11.0")
      , Tuple (name "elmish-html") (version "0.10.0")
      , Tuple (name "elmish-testing-library") (version "0.3.2")
      , Tuple (name "elmish-time-machine") (version "0.4.2")
      , Tuple (name "whine-core") (version "0.0.34")
      ]
    packageSet = PackageSet.PackageSet
      { version: version "79.0.0"
      , compiler: version "0.15.15"
      , published: Utils.unsafeDate "2026-07-27"
      , packages
      }
    manifests = manifestIndex
      [ Utils.unsafeManifest "elmish" "0.13.0" []
      , Utils.unsafeManifest "elmish" "0.14.0" []
      , Utils.unsafeManifest "elmish-html" "0.10.0" [ Tuple "elmish" ">=0.10.0 <0.14.0" ]
      , Utils.unsafeManifest "elmish-html" "0.11.1" [ Tuple "elmish" ">=0.13.0 <0.14.0" ]
      , Utils.unsafeManifest "elmish-html" "0.12.0" [ Tuple "elmish" ">=0.14.0 <0.15.0" ]
      , Utils.unsafeManifest "elmish-hooks" "0.11.0" [ Tuple "elmish" ">=0.13.0 <0.14.0" ]
      , Utils.unsafeManifest "elmish-time-machine" "0.4.2"
          [ Tuple "elmish" ">=0.13.0 <0.14.0"
          , Tuple "elmish-hooks" ">=0.11.0 <0.12.0"
          , Tuple "elmish-html" ">=0.9.0 <0.11.0"
          ]
      , Utils.unsafeManifest "elmish-enzyme" "0.1.1" [ Tuple "elmish" ">=0.8.0 <0.14.0" ]
      , Utils.unsafeManifest "elmish-testing-library" "0.3.2" [ Tuple "elmish" ">=0.8.0 <0.14.0" ]
      , Utils.unsafeManifest "whine-core" "0.0.34" []
      ]
    candidates = Map.fromFoldable
      [ candidate "elmish" [ "0.14.0" ]
      , candidate "elmish-html" [ "0.12.0", "0.11.1" ]
      ]
  { candidates, manifests, packageSet }

-- | An oracle reproducing the compiler behavior of package set 79: the
-- | elmish@0.14.0 upgrade breaks the unchanged elmish-hooks, elmish-html's
-- | latest version requires the new elmish, and only removing elmish-hooks
-- | (and its dependant elmish-time-machine) lets the coordinated upgrade
-- | compile.
elmishOracle :: PackageSet -> ChangeSet -> Run (EXCEPT String + ()) Planner.ProbeResult
elmishOracle _ changes = do
  let removed package = Map.lookup (name package) changes == Just Remove
  let updatedTo package selected = Map.lookup (name package) changes == Just (Update (version selected))
  let
    hooksError = String.joinWith "\n"
      [ "Error 1 of 1"
      , "  Module: Elmish.Hooks"
      , "  File: scratch/package-sets/packages/elmish-hooks@0.11.0/src/Elmish/Hooks.purs"
      , "  Message: Unknown value fork"
      ]
  let
    htmlError = String.joinWith "\n"
      [ "Error 1 of 1"
      , "  Module: Elmish.HTML"
      , "  File: scratch/package-sets/packages/elmish-html@0.12.0/src/Elmish/HTML.purs"
      ]
  if removed "elmish-hooks" && removed "elmish-time-machine" then pure Planner.Compiles
  else if updatedTo "elmish" "0.14.0" then pure $ Planner.CompilationFailure hooksError
  else if updatedTo "elmish-html" "0.12.0" then pure $ Planner.CompilationFailure htmlError
  else pure Planner.Compiles

candidate :: String -> Array String -> Tuple PackageName (NonEmptySet Version)
candidate packageName versions = Tuple (name packageName) (versionSet (map version versions))

versionSet :: Array Version -> NonEmptySet Version
versionSet versions = Utils.fromJust "Expected non-empty candidate versions." (NonEmptySet.fromFoldable versions)

versionMap :: Array (Tuple String String) -> Map PackageName Version
versionMap = Map.fromFoldable <<< map (bimap name version)

fileError :: String -> String -> String
fileError packageName packageVersion = String.joinWith "\n"
  [ "Error 1 of 1"
  , "  File: packages/" <> packageName <> "@" <> packageVersion <> "/src/Main.purs"
  , "  Message: Unknown value"
  ]

manifestIndex :: Array Manifest -> ManifestIndex
manifestIndex manifests = Utils.fromRight "Could not build planner test manifest index." $
  ManifestIndex.fromSet IgnoreRanges (Set.fromFoldable manifests)

mkPackageSet :: Map PackageName Version -> PackageSet
mkPackageSet packages = PackageSet.PackageSet
  { version: version "1.0.0"
  , compiler: version "0.15.10"
  , published: Utils.unsafeDate "2024-01-01"
  , packages
  }

name :: String -> PackageName
name = Utils.unsafePackageName

version :: String -> Version
version = Utils.unsafeVersion
