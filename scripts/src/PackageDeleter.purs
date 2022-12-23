module Registry.Scripts.PackageDeleter where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.String as String
import Data.Tuple (uncurry)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Node.Path as Path
import Node.Process as Process
import Registry.App.CLI.Git (CommitMode(..))
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, LOG_EXCEPT, LogVerbosity(..))
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (PullMode(..), REGISTRY, WriteStrategy(..))
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.Constants as Constants
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit (GitHubToken)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Run.Except

data DeleteMode = File FilePath | Package PackageName Version

derive instance Eq DeleteMode

type DeletePackages = Map PackageName (Array Version)

deletePackagesCodec :: JsonCodec DeletePackages
deletePackagesCodec = Internal.Codec.packageMap (CA.array Version.codec)

parser :: ArgParser DeleteMode
parser = Arg.choose "command"
  [ Arg.argument [ "--file" ]
      """Delete package versions from a JSON file like: { "prelude": [ "1.0.0", "1.1.1" ] }"""
      # Arg.unformat "FILE_PATH" pure
      # map File
  , Arg.argument [ "--package" ]
      "Delete the indicated package at the given version, separated by '@'"
      # Arg.unformat "NAME@VERSION" parsePackage
      # map (uncurry Package)
  ]
  where
  parsePackage :: String -> Either String (Tuple PackageName Version)
  parsePackage input = do
    let split = String.split (String.Pattern "@") input
    case Array.length split of
      0 -> Left "Expected package@version but received nothing."
      2 -> do
        rawPackage <- note "Unexpected error" (Array.index split 0)
        package <- lmap (append ("Failed to parse package name '" <> rawPackage <> "': ")) (PackageName.parse rawPackage)
        rawVersion <- note "Unexpected error" (Array.index split 1)
        version <- lmap (append ("Failed to parse version '" <> rawVersion <> "': ")) (Version.parse rawVersion)
        pure $ Tuple package version
      _ -> Left $ "Expected package@version but received an invalid format: " <> input

main :: Effect Unit
main = launchAff_ do
  args <- Array.drop 2 <$> liftEffect Process.argv
  let description = "A script for deleting registry packages."
  mode <- case Arg.parseArgs "package-deleter" description parser args of
    Left err -> Console.log (Arg.printArgError err) *> liftEffect (Process.exit 1)
    Right command -> pure command

  -- Environment
  _ <- Env.loadEnvFile ".env"
  token <- Env.lookupRequired Env.pacchettibottiToken
  spacesKey <- Env.lookupRequired Env.spacesKey
  spacesSecret <- Env.lookupRequired Env.spacesSecret

  -- Registry
  let
    registry = Path.concat [ scratchDir, "registry" ]
    registryIndex = Path.concat [ scratchDir, "registry-index" ]
    registryEnv =
      { legacyPackageSets: Path.concat [ scratchDir, "package-sets" ]
      , registry
      , registryIndex
      , pullMode: OnlyClean
      , writeStrategy: WriteCommitPush token
      , timers: unsafePerformEffect Registry.newTimers
      }

  -- GitHub
  octokit <- liftEffect $ Octokit.newOctokit token

  -- Caching
  let cacheDir = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cacheDir
  githubCacheRef <- Cache.newCacheRef

  -- Logging
  now <- liftEffect nowUTC
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  let logFile = "package-set-deleter-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]

  deletions <- case mode of
    Package name version -> pure $ Map.singleton name [ version ]
    File path -> liftAff (readJsonFile deletePackagesCodec path) >>= case _ of
      Left err -> Console.log err *> liftEffect (Process.exit 1)
      Right values -> pure values

  deleter { registry, registryIndex, token, mode: CommitAndPush } deletions
    -- App effects
    # Registry.runRegistry (Registry.handleRegistryGit registryEnv)
    # Storage.runStorage (Storage.handleStorageS3 { key: spacesKey, secret: spacesSecret })
    # GitHub.runGitHub (GitHub.handleGitHubOctokit octokit)
    -- Caching
    # Storage.runStorageCacheFs cacheDir
    # GitHub.runGitHubCacheMemoryFs githubCacheRef cacheDir
    -- Logging
    # Run.Except.catchAt Log._logExcept (\msg -> Log.error msg *> Run.liftEffect (Process.exit 1))
    # Log.runLog (\log -> Log.handleLogTerminal Normal log *> Log.handleLogFs Verbose logPath log)
    -- Base effects
    # Run.runBaseAff'

type DeleterArgs =
  { registry :: FilePath
  , registryIndex :: FilePath
  , token :: GitHubToken
  , mode :: CommitMode
  }

deleter :: forall r. DeleterArgs -> Map PackageName (Array Version) -> Run (REGISTRY + STORAGE + LOG + LOG_EXCEPT + AFF + EFFECT + r) Unit
deleter env deletions = do
  Log.info $ Array.fold
    [ "Deleting package versions:"
    , do
        let foldFn name versions = "\n  - " <> PackageName.print name <> " " <> String.joinWith ", " (map Version.print versions)
        foldMapWithIndex foldFn deletions
    ]

  Registry.fetchGitHubRepo Constants.registry OnlyClean env.registry
  Registry.fetchGitHubRepo Constants.manifestIndex OnlyClean env.registryIndex

  forWithIndex_ deletions \name versions ->
    for_ versions \version -> do
      result <- Run.Except.runExcept $ deleteVersion name version
      let printed = Version.print version
      case result of
        Left err -> do
          Log.error $ "Failed to delete " <> printed <> ": " <> err
        Right _ ->
          Log.info $ "Successfully removed " <> printed

  commitMetadata <- Run.liftAff $ Except.runExceptT do
    let message = "Remove some package versions from metadata."
    let paths = String.Pattern "*.json"
    Git.pacchettiBottiCommitRegistry env.registry { token: env.token, mode: env.mode, paths, message }

  case commitMetadata of
    Left err -> Log.error $ "Failed to commit metadata!\n" <> err
    Right _ -> pure unit

  commitIndex <- Run.liftAff $ Except.runExceptT do
    let message = "Remove some package versions from manifest index."
    let paths = String.Pattern "."
    Git.pacchettiBottiCommitRegistryIndex env.registryIndex { token: env.token, mode: env.mode, paths, message }

  case commitIndex of
    Left err -> Log.error $ "Failed to commit manifest index!\n" <> err
    Right _ -> pure unit

  Log.info "Finished."

deleteVersion :: forall r. PackageName -> Version -> Run (REGISTRY + STORAGE + LOG + EXCEPT String + r) Unit
deleteVersion name version = do
  let formatted = formatPackageVersion name version
  Log.info $ "Deleting " <> formatted
  Storage.deleteTarball name version
  Log.info $ "Updating metadata for " <> formatted
  Registry.readMetadata name >>= case _ of
    Nothing -> Run.Except.throw $ "Could not update metadata for " <> formatted <> " because no existing metadata was found."
    Just (Metadata old) -> do
      let new = Metadata $ old { published = Map.delete version old.published, unpublished = Map.delete version old.unpublished }
      Registry.writeMetadata name new
      Registry.deleteManifest name version
