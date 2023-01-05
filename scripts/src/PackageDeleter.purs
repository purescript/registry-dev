module Registry.Scripts.PackageDeleter where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Control.Apply (lift2)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Data.String as String
import Data.Tuple (uncurry)
import Effect.Class.Console as Console
import Node.Path as Path
import Node.Process as Process
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Git (GitEnv, PullMode(..), WriteMode(..))
import Registry.App.Effect.Git as Git
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, LogVerbosity(..))
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

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
  s3 <- lift2 { key: _, secret: _ } (Env.lookupRequired Env.spacesKey) (Env.lookupRequired Env.spacesSecret)

  -- Git
  debouncer <- Git.newDebouncer
  let
    gitEnv :: WriteMode -> GitEnv
    gitEnv writeMode =
      { write: writeMode
      , pull: OnlyClean
      , repos: Git.defaultRepos
      , workdir: scratchDir
      , debouncer
      }

  -- GitHub
  octokit <- Octokit.newOctokit token

  -- Caching
  let cache = Path.concat [ scratchDir, ".cache" ]
  FS.Extra.ensureDirectory cache
  githubCacheRef <- Cache.newCacheRef
  registryCacheRef <- Cache.newCacheRef

  -- Logging
  now <- nowUTC
  let logDir = Path.concat [ scratchDir, "logs" ]
  FS.Extra.ensureDirectory logDir
  let logFile = "package-set-deleter-" <> String.take 19 (Formatter.DateTime.format Internal.Format.iso8601DateTime now) <> ".log"
  let logPath = Path.concat [ logDir, logFile ]

  deletions <- case mode of
    Package name version -> pure $ Map.singleton name [ version ]
    File path -> liftAff (readJsonFile deletePackagesCodec path) >>= case _ of
      Left err -> Console.log err *> liftEffect (Process.exit 1)
      Right values -> pure values

  let
    interpret gitMode =
      Registry.interpret (Registry.handle registryCacheRef)
        >>> Storage.interpret (Storage.handleS3 { s3, cache })
        >>> GitHub.interpret (GitHub.handle { octokit, cache, ref: githubCacheRef })
        >>> Git.interpret (Git.handle (gitEnv gitMode))
        >>> Log.interpret (\log -> Log.handleTerminal Normal log *> Log.handleFs Verbose logPath log)
        >>> Run.runBaseAff'

  -- We run deletions *without* committing, because we'll do it in bulk later.
  interpret ReadOnly do
    Log.info $ Array.fold
      [ "Deleting package versions:"
      , do
          let foldFn name versions = "\n  - " <> PackageName.print name <> " " <> String.joinWith ", " (map Version.print versions)
          foldMapWithIndex foldFn deletions
      ]

    forWithIndex_ deletions \name versions ->
      for_ versions \version -> do
        result <- Except.runExcept $ deleteVersion name version
        let printed = Version.print version
        case result of
          Left err -> do
            Log.error $ "Failed to delete " <> printed <> ": " <> err
          Right _ ->
            Log.info $ "Successfully removed " <> printed

    Log.info "Finished."

  -- Then we add our commits with committing enabled.
  interpret (CommitAs (Git.pacchettibottiCommitter token)) do
    Git.commit Git.CommitMetadataIndex "Remove some package versions from metadata." >>= case _ of
      Left error -> Log.error $ "Failed to commit metadata: " <> error
      Right _ -> pure unit

    Git.commit Git.CommitManifestIndex "Remove some package versions from manifest index." >>= case _ of
      Left error -> Log.error $ "Failed to commit manifest index: " <> error
      Right _ -> pure unit

deleteVersion :: forall r. PackageName -> Version -> Run (REGISTRY + STORAGE + LOG + EXCEPT String + r) Unit
deleteVersion name version = do
  let formatted = formatPackageVersion name version
  Log.info $ "Deleting " <> formatted
  Storage.delete name version
  Log.info $ "Updating metadata for " <> formatted
  Registry.readMetadata name >>= case _ of
    Nothing -> Except.throw $ "Could not update metadata for " <> formatted <> " because no existing metadata was found."
    Just (Metadata old) -> do
      let new = Metadata $ old { published = Map.delete version old.published, unpublished = Map.delete version old.unpublished }
      Registry.writeMetadata name new
      Registry.deleteManifest name version
