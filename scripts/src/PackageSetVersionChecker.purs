-- | Compile-verified report of pending package set upgrades.
-- |
-- | This script plans upgrades over every registry version newer than its
-- | version in the latest package set, exactly as the package set updater
-- | would, and reports the results as Markdown:
-- |
-- |  - upgrades that compile and can be submitted automatically, and
-- |  - upgrades that cannot be applied automatically because they require
-- |    removing packages from the set, with compile-verified removal payloads
-- |    that a Registry Trustee can review and submit.
-- |
-- | Packages that have never appeared in a package set are not independently
-- | proposed by this report, though one may be included when a proposed
-- | upgrade depends on it; discovering those packages is tracked as separate
-- | follow-up work. Upgrades that would require downgrading a package in the
-- | set are likewise not analyzed: the registry API rejects downgrades, so
-- | supporting them needs its own analysis and authenticated submission path.
-- |
-- | Run via Nix:
-- |   nix run .#package-set-version-checker                        # Print the report
-- |   nix run .#package-set-version-checker -- --output report.md  # Also write it to a file
module Registry.Scripts.PackageSetVersionChecker where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Effect.Class.Console as Console
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env (RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets (PACKAGE_SETS)
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Registry (REGISTRY_READ)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.App.PackageSetPlanner (Plan, RemovalAnalysis(..), RemovalReport)
import Registry.App.PackageSetPlanner as Planner
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Internal.Codec as Internal.Codec
import Registry.Operation (PackageSetOperation(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.PackageSet (PackageSet(..))
import Registry.Scripts.PackageSetUpdater as PackageSetUpdater
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

parser :: ArgParser (Maybe FilePath)
parser = Arg.argument [ "--output" ] "Also write the Markdown report to this file." # Arg.optional

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv

  let description = "Report pending package set upgrades, compile-verified."
  output <- case Arg.parseArgs "package-set-version-checker" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit' 1)
    Right parsed -> pure parsed

  Env.loadEnvFile ".env"
  resourceEnv <- Env.lookupResourceEnv

  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache
  registryCacheRef <- Cache.newCacheRef

  debouncer <- Registry.newDebouncer

  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { jobId: Nothing
      , pull: Git.Autostash
      , write: Registry.ReadOnly
      , repos: Registry.defaultRepos
      , workdir: scratchDir
      , debouncer
      , cacheRef: registryCacheRef
      }

  runVersionCheck
    # PackageSets.interpret (PackageSets.handle { workdir: scratchDir })
    # Except.runExcept
    # Registry.interpretRead (Registry.handleRead registryEnv)
    # Storage.interpret (Storage.handleReadOnly cache)
    # Env.runResourceEnv resourceEnv
    # Log.interpret (Log.handleTerminal Normal)
    # Run.runBaseAff'
    >>= case _ of
      Left err -> do
        Console.error $ "Error: " <> err
        liftEffect $ Process.exit' 1
      Right Nothing ->
        Console.log "No package set found; no report was generated."
      Right (Just results) -> do
        generatedAt <- nowUTC
        let markdown = renderReport generatedAt results
        Console.log markdown
        for_ output \path -> FS.Aff.writeTextFile UTF8 path (markdown <> "\n")

type VersionCheckEffects = (REGISTRY_READ + PACKAGE_SETS + STORAGE + RESOURCE_ENV + LOG + EXCEPT String + AFF + EFFECT + ())

type CheckResults =
  { packageSet :: PackageSet
  , plan :: Plan
  , removals :: Array RemovalReport
  }

runVersionCheck :: Run VersionCheckEffects (Maybe CheckResults)
runVersionCheck = do
  Log.info "Registry Version Check: planning pending package set upgrades..."
  Registry.readLatestPackageSet >>= case _ of
    Nothing -> do
      Log.warn "No package set found, skipping version check."
      pure Nothing
    Just packageSet -> do
      metadata <- Registry.readAllMetadata
      manifests <- Registry.readAllManifests
      let candidates = PackageSetUpdater.plannerCandidates packageSet metadata manifests Map.empty
      Log.info $ "Compile-planning upgrades for " <> show (Map.size candidates) <> " candidate packages..."
      plan <- Planner.planUpgrades Planner.probeAtomic packageSet manifests Set.empty candidates
      removals <- Planner.analyzeRemovals Planner.probeAtomic packageSet manifests plan
      pure $ Just { packageSet, plan, removals }

-- | Render the version check results as a Markdown report suitable for a
-- | GitHub issue.
renderReport :: DateTime -> CheckResults -> String
renderReport generatedAt { packageSet: PackageSet set, plan, removals } = String.joinWith "\n" $ Array.concat
  [ [ "# Package set version check"
    , ""
    , "Package set: `" <> Version.print set.version <> "` (PureScript `" <> Version.print set.compiler <> "`)"
    , "Generated: `" <> Internal.Codec.formatIso8601 generatedAt <> "`"
    , ""
    ]
  , if not plan.truncated then []
    else
      [ "⚠️ The planner exhausted its probe budget, so this report may be incomplete. Verified results below still compiled exactly."
      , ""
      ]
  , automaticSection
  , manualSection
  ]
  where
  automaticSection = Array.concat
    [ [ "## Verified automatic upgrades", "" ]
    , if Map.isEmpty plan.verified then
        [ "No pending upgrades could be compile-verified for automatic submission.", "" ]
      else Array.concat
        [ map renderUpgrade (Map.toUnfoldable plan.verified)
        , [ ""
          , "This exact payload compiled as a whole set and is eligible for automatic submission. The package set updater (`nix run .#package-set-updater -- --submit`) replans against the latest package set before submitting, so its payload may differ if the registry has moved on:"
          , ""
          , payloadFence plan.verified Set.empty
          , ""
          ]
        ]
    ]

  renderUpgrade (Tuple name version) = do
    let from = maybe "new" Version.print (Map.lookup name set.packages)
    "- `" <> PackageName.print name <> "`: `" <> from <> "` → `" <> Version.print version <> "`"

  manualSection = Array.concat
    [ [ "## Upgrades requiring manual intervention", "" ]
    , if Array.null removals then
        [ "All pending upgrades were included in the automatic payload. Nothing requires trustee attention." ]
      else Array.concatMap renderRemoval removals
    ]

  renderRemoval report = Array.concat
    [ [ "### " <> String.joinWith ", " (map (\(Tuple name version) -> "`" <> formatPackageVersion name version <> "`") (Map.toUnfoldable report.targets))
      , ""
      ]
    , case report.analysis of
        RemovalsVerified { removed, updates } ->
          [ "Removing " <> printNames removed <> " lets these upgrades compile. This exact payload — the upgrades above together with every verified automatic upgrade — was compile-verified against package set `" <> Version.print set.version <> "` and can be submitted by a Registry Trustee:"
          , ""
          , payloadFence updates removed
          ]
        RemovalsUnverified removedPackages ->
          [ "Removing " <> printNames removedPackages <> " was suggested by compiler output, but the resulting set did not compile. Do not submit this without further investigation."
          ]
        RemovalsNotAnalyzed reason ->
          [ "No removal payload could be recommended: " <> reason ]
    , if Array.null report.blockers then []
      else [ "", "Blocked by: " <> printNames (Set.fromFoldable report.blockers) ]
    , case report.evidence of
        Nothing -> [ "" ]
        Just evidence ->
          [ ""
          , "<details><summary>Compiler evidence</summary>"
          , ""
          , "```"
          , evidence
          , "```"
          , ""
          , "</details>"
          , ""
          ]
    ]

  printNames :: Set PackageName -> String
  printNames names = String.joinWith ", " $ map (\name -> "`" <> PackageName.print name <> "`") (Array.fromFoldable names)

  payloadFence :: Map PackageName Version -> Set PackageName -> String
  payloadFence updates removedPackages = do
    let
      removalChanges :: Map PackageName (Maybe Version)
      removalChanges = Map.fromFoldable $ map (\name -> Tuple name Nothing) (Array.fromFoldable removedPackages :: Array PackageName)
      operation = PackageSetUpdate
        { compiler: Nothing
        , packages: Map.union removalChanges (map Just updates)
        }
    String.joinWith "\n"
      [ "```json"
      , printJson Operation.packageSetOperationCodec operation
      , "```"
      ]
