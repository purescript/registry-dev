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
import Node.Path as Path
import Node.Process as Process
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets (Change(..), PACKAGE_SETS)
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Format as Internal.Format
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

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
  args <- Array.drop 2 <$> liftEffect Process.argv
  let description = "A script for updating the package sets."
  mode <- case Arg.parseArgs "package-set-updater" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit 1)
    Right command -> pure command

  -- Environment
  _ <- Env.loadEnvFile ".env"

  { token, write } <- case mode of
    GeneratePackageSet -> do
      Env.lookupOptional Env.githubToken >>= case _ of
        Nothing -> do
          token <- Env.lookupRequired Env.pacchettibottiToken
          pure { token, write: Registry.ReadOnly }
        Just token ->
          pure { token, write: Registry.ReadOnly }
    CommitPackageSet -> do
      token <- Env.lookupRequired Env.pacchettibottiToken
      pure { token, write: Registry.CommitAs (Git.pacchettibottiCommitter token) }

  -- Package sets
  let packageSetsEnv = { workdir: Path.concat [ scratchDir, "package-set-build" ] }

  -- GitHub
  octokit <- Octokit.newOctokit token

  -- Caching
  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache
  githubCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef

  -- Registry
  debouncer <- Registry.newDebouncer
  let
    registryEnv :: Registry.RegistryEnv
    registryEnv =
      { write
      , pull: Git.ForceClean
      , repos: Registry.defaultRepos
      , workdir: scratchDir
      , debouncer
      , cacheRef: registryCacheRef
      }

  -- Logging
  now <- nowUTC
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  let logFile = "package-set-updater-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]

  updater
    # PackageSets.interpret (PackageSets.handle packageSetsEnv)
    # Registry.interpret (Registry.handle registryEnv)
    # Storage.interpret (Storage.handleReadOnly cache)
    # GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
    # Except.catch (\msg -> Log.error msg *> Run.liftEffect (Process.exit 1))
    # Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
    # Run.runBaseAff'

updater :: forall r. Run (REGISTRY + PACKAGE_SETS + LOG + EXCEPT String + AFF + EFFECT + r) Unit
updater = do
  prevPackageSet <- Registry.readLatestPackageSet >>= case _ of
    Nothing -> Except.throw "No previous package set found, cannot continue."
    Just set -> pure set

  PackageSets.validatePackageSet prevPackageSet

  let compiler = (un PackageSet prevPackageSet).compiler

  Log.info $ "Using compiler " <> Version.print compiler

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
    PackageSets.upgradeSequential prevPackageSet compiler (map (maybe Remove Update) candidates.accepted) >>= case _ of
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

findRecentUploads :: forall r. Hours -> Run (REGISTRY + EXCEPT String + EFFECT + r) RecentUploads
findRecentUploads limit = do
  allMetadata <- Registry.readAllMetadata
  now <- nowUTC

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
