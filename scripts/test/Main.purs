module Test.Registry.Scripts.Main (main) where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.Time.Duration (Hours(..))
import Registry.App.PackageSetPlanner as Planner
import Registry.Manifest (Manifest)
import Registry.ManifestIndex (IncludeRanges(..), ManifestIndex)
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata (Metadata)
import Registry.Metadata as Metadata
import Registry.PackageName (PackageName)
import Registry.PackageSet (PackageSet)
import Registry.PackageSet as PackageSet
import Registry.Scripts.PackageSetUpdater as PackageSetUpdater
import Registry.Scripts.PackageSetVersionChecker as PackageSetVersionChecker
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Registry.Version (Version)
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Spec.describe "PackageSetUpdater candidate selection" do
    Spec.it "seeds additions from recent uploads of packages not in the set" do
      let
        oldTime = Utils.unsafeDateTime "2023-12-30T00:00:00.000Z"
        now = Utils.unsafeDateTime "2024-01-02T00:00:00.000Z"
        metadata = Map.fromFoldable
          [ Utils.unsafeMetadata "prelude" [ Tuple "1.0.0" [ "0.15.10" ], Tuple "2.0.0" [ "0.15.10" ] ]
          , withPublishedTime oldTime $ Utils.unsafeMetadata "old-upload" [ Tuple "1.0.0" [ "0.15.10" ] ]
          , Utils.unsafeMetadata "new-package" [ Tuple "1.0.0" [ "0.15.10" ] ]
          ]
        packageSet = mkPackageSet $ Map.singleton (name "prelude") (version "1.0.0")

      -- `prelude` is excluded because it is already in the set: pending
      -- upgrades of set members are always planner candidates and never
      -- expire. `old-upload` is excluded because it is not a recent upload.
      PackageSetUpdater.recentAdditionSeeds now (Hours 24.0) packageSet metadata
        `Assert.shouldEqual` Map.singleton (name "new-package") (version "1.0.0")

    Spec.it "uses all newer existing versions and closes recent additions by dependency name" do
      let
        packageSet = mkPackageSet $ Map.singleton (name "existing") (version "1.0.0")
        metadata = Map.fromFoldable
          [ Utils.unsafeMetadata "existing" [ Tuple "1.0.0" [ "0.1.0" ], Tuple "2.0.0" [ "0.1.0" ], Tuple "3.0.0" [ "0.1.0" ] ]
          , Utils.unsafeMetadata "recent" [ Tuple "1.0.0" [ "0.1.0" ] ]
          , Utils.unsafeMetadata "closure-dep" [ Tuple "1.0.0" [ "0.1.0" ], Tuple "2.0.0" [ "0.1.0" ] ]
          , Utils.unsafeMetadata "upgrade-dep" [ Tuple "1.0.0" [ "0.1.0" ] ]
          ]
        manifests = manifestIndex
          [ Utils.unsafeManifest "existing" "2.0.0" []
          , Utils.unsafeManifest "existing" "3.0.0" [ Tuple "upgrade-dep" ">=99.0.0 <100.0.0" ]
          , Utils.unsafeManifest "recent" "1.0.0" [ Tuple "closure-dep" ">=99.0.0 <100.0.0" ]
          , Utils.unsafeManifest "closure-dep" "1.0.0" []
          , Utils.unsafeManifest "closure-dep" "2.0.0" []
          , Utils.unsafeManifest "upgrade-dep" "1.0.0" []
          ]
        recent = Map.singleton (name "recent") (version "1.0.0")
        candidates = PackageSetUpdater.plannerCandidates packageSet metadata manifests recent
        byName = map (Array.reverse <<< Array.fromFoldable) candidates

      Map.lookup (name "existing") byName `Assert.shouldEqual` Just [ version "3.0.0", version "2.0.0" ]
      Map.lookup (name "recent") byName `Assert.shouldEqual` Just [ version "1.0.0" ]
      -- Support packages are only ever used at their latest version, so only
      -- that version is a candidate.
      Map.lookup (name "closure-dep") byName `Assert.shouldEqual` Just [ version "2.0.0" ]
      -- `upgrade-dep` is not a recent upload and has never been in a set, but
      -- the `existing` upgrade requires it, so the closure includes it:
      -- recency only gates which additions are seeded, never dependencies.
      Map.lookup (name "upgrade-dep") byName `Assert.shouldEqual` Just [ version "1.0.0" ]

  Spec.describe "PackageSetVersionChecker report" do
    Spec.it "renders an actionable version check report" do
      -- A plan and removal analysis matching the Elmish package-set-79
      -- scenario, whose planner behavior is tested end-to-end in
      -- Test.Registry.App.PackageSetPlanner. Here we only test rendering.
      let
        packageSet = PackageSet.PackageSet
          { version: version "79.0.0"
          , compiler: version "0.15.15"
          , published: Utils.unsafeDate "2026-07-27"
          , packages: Map.fromFoldable
              [ Tuple (name "elmish") (version "0.13.0")
              , Tuple (name "elmish-hooks") (version "0.11.0")
              , Tuple (name "elmish-html") (version "0.10.0")
              , Tuple (name "elmish-time-machine") (version "0.4.2")
              , Tuple (name "whine-core") (version "0.0.34")
              ]
          }
        hooksEvidence = String.joinWith "\n"
          [ "Error 1 of 1"
          , "  Module: Elmish.Hooks"
          , "  File: scratch/package-sets/packages/elmish-hooks@0.11.0/src/Elmish/Hooks.purs"
          , "  Message: Unknown value fork"
          ]
        blockedTargets = versionMap [ Tuple "elmish" "0.14.0", Tuple "elmish-html" "0.12.0" ]
        -- The verified payload includes an upgrade unrelated to the blocked
        -- group; the removal probe compiled it too, so the trustee payload
        -- must carry it.
        verified = versionMap [ Tuple "elmish-html" "0.11.1", Tuple "whine-core" "0.0.35" ]
        plan =
          { blocked: [ { blockers: [ name "elmish-hooks" ], evidence: Just hooksEvidence, targets: blockedTargets } ]
          , probes: 10
          , truncated: false
          , verified
          }
        removals =
          [ { analysis: Planner.RemovalsVerified
                { removed: Set.fromFoldable [ name "elmish-hooks", name "elmish-time-machine" ]
                , updates: Map.union blockedTargets verified
                }
            , blockers: [ name "elmish-hooks" ]
            , evidence: Just hooksEvidence
            , targets: blockedTargets
            }
          ]
        generatedAt = Utils.unsafeDateTime "2026-07-27T12:00:00.000Z"
        markdown = PackageSetVersionChecker.renderReport generatedAt { packageSet, plan, removals }
        shouldMention excerpt = String.contains (String.Pattern excerpt) markdown `Assert.shouldEqual` true

      -- The automatic payload and its submission command.
      shouldMention "`elmish-html`: `0.10.0` → `0.11.1`"
      shouldMention "\"elmish-html\": \"0.11.1\""
      shouldMention "nix run .#package-set-updater -- --submit"
      -- The trustee removal payload.
      shouldMention "\"elmish-hooks\": null"
      shouldMention "\"elmish-time-machine\": null"
      shouldMention "\"elmish\": \"0.14.0\""
      shouldMention "\"elmish-html\": \"0.12.0\""
      -- The compiler evidence that motivated the removal.
      shouldMention "elmish-hooks@0.11.0/src"
      -- The trustee payload is the exact compiled update map, so it includes
      -- the verified `whine-core` upgrade even though it is unrelated to the
      -- blocked group.
      case Array.index (String.split (String.Pattern "## Upgrades requiring manual intervention") markdown) 1 of
        Nothing -> Assert.fail "Expected a manual intervention section."
        Just manualSection -> String.contains (String.Pattern "\"whine-core\": \"0.0.35\"") manualSection `Assert.shouldEqual` true

versionMap :: Array (Tuple String String) -> Map PackageName Version
versionMap = Map.fromFoldable <<< map (bimap name version)

manifestIndex :: Array Manifest -> ManifestIndex
manifestIndex manifests = Utils.fromRight "Could not build scripts test manifest index." $
  ManifestIndex.fromSet IgnoreRanges (Set.fromFoldable manifests)

mkPackageSet :: Map PackageName Version -> PackageSet
mkPackageSet packages = PackageSet.PackageSet
  { version: version "1.0.0"
  , compiler: version "0.15.10"
  , published: Utils.unsafeDate "2024-01-01"
  , packages
  }

withPublishedTime :: forall a. DateTime -> Tuple a Metadata -> Tuple a Metadata
withPublishedTime publishedTime (Tuple package (Metadata.Metadata metadata)) =
  Tuple package $ Metadata.Metadata $ metadata
    { published = map (_ { publishedTime = publishedTime }) metadata.published }

name :: String -> PackageName
name = Utils.unsafePackageName

version :: String -> Version
version = Utils.unsafeVersion
