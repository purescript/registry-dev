module Registry.Scripts.PackageSetUpdater where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime as DateTime
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.Number.Format as Number.Format
import Data.String as String
import Data.Time.Duration (Hours(..))
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Node.Path as Path
import Node.Process as Process
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, LOG_EXCEPT, LogVerbosity(..))
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets (Change(..), PACKAGE_SETS)
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Registry (PullMode(..), REGISTRY, RegistryEnv, WriteStrategy(..))
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Run (EFFECT, Run)
import Run as Run
import Run.Except as Run.Except

data PublishMode = GeneratePackageSet | CommitPackageSet

derive instance Eq PublishMode

parser :: ArgParser PublishMode
parser = Arg.choose "command"
  [ Arg.flag [ "generate" ]
      "Generate a new package set without committing the results."
      $> GeneratePackageSet
  , Arg.flag [ "commit" ]
      "Generate a new package set and commit the results."
      $> CommitPackageSet
  ]

main :: Effect Unit
main = Aff.launchAff_ do
  _ <- Env.loadEnvFile ".env"
  env <- liftEffect Env.readEnvVars

  FS.Extra.ensureDirectory scratchDir

  args <- Array.drop 2 <$> liftEffect Process.argv
  let description = "A script for updating the package sets."
  mode <- case Arg.parseArgs "package-set-updater" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit 1)
    Right command -> pure command

  Console.log "Starting package set publishing..."

  { octokit, writeStrategy } <- case mode of
    GeneratePackageSet -> do
      token <- case env.githubToken of
        Nothing -> Aff.throwError $ Aff.error "GITHUB_TOKEN not defined in the environment."
        Just token -> pure token
      octokit <- liftEffect $ Octokit.newOctokit token
      pure { writeStrategy: Write, octokit }

    CommitPackageSet -> do
      token <- case env.pacchettibottiToken of
        Nothing -> Aff.throwError $ Aff.error "PACCHETTIBOTTI_TOKEN not defined in the environment."
        Just token -> pure token
      octokit <- liftEffect $ Octokit.newOctokit token
      pure { writeStrategy: WriteCommitPush token, octokit }

  let
    registryEnv :: RegistryEnv
    registryEnv =
      { legacyPackageSets: Path.concat [ scratchDir, "package-sets" ]
      , registry: Path.concat [ scratchDir, "registry" ]
      , registryIndex: Path.concat [ scratchDir, "registry-index" ]
      , pullMode: ForceClean
      , writeStrategy
      , timers: unsafePerformEffect Registry.newTimers
      }

  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir

  now <- liftEffect nowUTC
  let logFile = "package-set-updater-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]

  let cacheDir = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cacheDir
  githubCacheRef <- Cache.newCacheRef

  let packageSetsWorkDir = Path.concat [ scratchDir, "package-set-build" ]

  updater
    -- App effects
    # PackageSets.runPackageSets (PackageSets.handlePackageSetsAff { workdir: packageSetsWorkDir })
    # Registry.runRegistry (Registry.handleRegistryGit registryEnv)
    # Storage.runStorage Storage.handleStorageReadOnly
    # GitHub.runGitHub (GitHub.handleGitHubOctokit octokit)
    -- Caches
    # Storage.runStorageCacheFs cacheDir
    # GitHub.runGitHubCacheMemoryFs githubCacheRef cacheDir
    -- Logging
    # Run.Except.catchAt Log._logExcept (\msg -> Log.error msg *> Run.liftEffect (Process.exit 1))
    # Log.runLog (\log -> Log.handleLogTerminal Normal log *> Log.handleLogFs Verbose logPath log)
    # Run.runBaseAff'

updater :: forall r. Run (REGISTRY + PACKAGE_SETS + LOG + LOG_EXCEPT + EFFECT + r) Unit
updater = do
  prevPackageSet <- Registry.readLatestPackageSet >>= case _ of
    Nothing -> Log.exit "No previous package set found, cannot continue."
    Just set -> pure set

  PackageSets.validatePackageSet prevPackageSet

  let uploadHours = 24.0
  recentUploads <- findRecentUploads (Hours uploadHours)

  manifestIndex <- Registry.readAllManifests
  let candidates = PackageSets.validatePackageSetCandidates manifestIndex prevPackageSet (map Just recentUploads.eligible)
  unless (Map.isEmpty candidates.rejected) do
    Log.info $ "Some packages uploaded in the last " <> Number.Format.toString uploadHours <> " hours are not eligible for the automated package sets."
    Log.info $ PackageSets.printRejections candidates.rejected

  if Map.isEmpty candidates.accepted then do
    Log.info "No eligible additions, updates, or removals to produce a new package set."
  else do
    -- You can't remove packages via the automatic updater.
    let eligible = Map.catMaybes candidates.accepted
    let listPackages = foldMapWithIndex \name version -> [ formatPackageVersion name version ]
    Log.info $ "Found package versions eligible for inclusion in package set: " <> Array.foldMap (append "\n  - ") (listPackages eligible)
    PackageSets.upgradeSequential prevPackageSet (un PackageSet prevPackageSet).compiler (map (maybe Remove Update) candidates.accepted) >>= case _ of
      Nothing -> do
        Log.info "No packages could be added to the set. All packages failed."
      Just { failed, succeeded, result } -> do
        let
          listChanges = foldMapWithIndex \name -> case _ of
            Remove -> []
            Update version -> [ formatPackageVersion name version ]
        unless (Map.isEmpty failed) do
          Log.info $ "Some packages could not be added to the set: " <> Array.foldMap (append "\n  - ") (listChanges failed)
        Log.info $ "New packages were added to the set: " <> Array.foldMap (append "\n  - ") (listChanges succeeded)
        -- We only include the successful changes in the commit message.
        let commitMessage = PackageSets.commitMessage prevPackageSet succeeded (un PackageSet result).version
        Registry.writePackageSet result commitMessage
        Log.info "Built and released a new package set! Now mirroring to the package-sets repo..."
        Registry.mirrorPackageSet result
        Log.info "Mirrored a new legacy package set."

type RecentUploads =
  { eligible :: Map PackageName Version
  , ineligible :: Map PackageName (NonEmptyArray Version)
  }

findRecentUploads :: forall r. Hours -> Run (REGISTRY + EFFECT + r) RecentUploads
findRecentUploads limit = do
  allMetadata <- Registry.readAllMetadata
  now <- Run.liftEffect nowUTC

  let
    uploads = Map.fromFoldable do
      Tuple name (Metadata metadata) <- Map.toUnfoldable allMetadata
      versions <- Array.fromFoldable $ NonEmptyArray.fromArray do
        Tuple version { publishedTime } <- Map.toUnfoldable metadata.published
        let diff = DateTime.diff now publishedTime
        guardA (diff <= limit)
        pure version
      pure (Tuple name versions)

    deduplicated = uploads # flip foldlWithIndex { ineligible: Map.empty, eligible: Map.empty } \name acc versions -> do
      let { init, last } = NonEmptyArray.unsnoc versions
      case NonEmptyArray.fromArray init of
        Nothing -> acc { eligible = Map.insert name last acc.eligible }
        Just entries -> acc { eligible = Map.insert name last acc.eligible, ineligible = Map.insert name entries acc.ineligible }

  pure deduplicated
