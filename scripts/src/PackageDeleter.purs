module Registry.Scripts.PackageDeleter where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Data.String as String
import Data.Tuple (uncurry)
import Effect.Class.Console as Console
import Node.Process as Process
import Registry.App.Effect.Log (LOG, LOG_EXCEPT)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY)
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE)
import Registry.App.Effect.Storage as Storage
import Registry.Internal.Codec as Internal.Codec
import Registry.PackageName as PackageName
import Registry.Version as Version
import Run (Run)

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
  pure unit

{-
  _ <- API.loadEnv
  FS.Extra.ensureDirectory API.scratchDir

  octokit <- liftEffect do
    token <- do
      result <- Process.lookupEnv "PACCHETTIBOTTI_TOKEN"
      maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment.") (pure <<< GitHubToken) result
    Octokit.newOctokit token

  cache <- Cache.useCache API.cacheDir
  metadataRef <- liftEffect $ Ref.new Map.empty

  let
    env :: Env
    env =
      { comment: \comment -> Console.log ("[COMMENT] " <> comment)
      , closeIssue: Console.log "Running locally, not closing issue..."
      , commitMetadataFile: \_ _ -> Console.log "Not using RegistryM to commit files." $> Right unit
      , commitIndexFile: \_ _ -> Console.log "Not using RegistryM to commit files." $> Right unit
      , commitPackageSetFile: \_ _ _ -> Console.log "Not committing package sets in package deleter." $> Right unit
      , uploadPackage: \_ _ -> Console.log $ "Not uploading packages in package deleter."
      , deletePackage: \_ -> Console.log $ "Not using registry delete package function."
      , packagesMetadata: metadataRef
      , cache
      , octokit
      , username: mempty
      , registry: Path.concat [ API.scratchDir, "registry" ]
      , registryIndex: Path.concat [ API.scratchDir, "registry-index" ]
      }

  runRegistryM env do
    API.fetchRegistry
    API.fetchRegistryIndex
    API.fillMetadataRef

    packages <- case mode of
      Package name version -> pure $ Map.singleton name [ version ]
      File path -> liftAff (readJsonFile deletePackagesCodec path) >>= case _ of
        Left err -> Console.log err *> liftEffect (Process.exit 1)
        Right values -> pure values

    { registry, registryIndex } <- ask

    Console.log $ "\n-----\nDELETING PACKAGE VERSIONS\n-----\n"
    Console.log $ "Writing metadata changes to " <> registry
    Console.log $ "Writing manifest index changes to " <> registryIndex

    forWithIndex_ packages \name versions -> do
      Console.log $ "Processing versions for " <> PackageName.print name

      for_ versions \version -> do
        result <- deleteVersion name version
        let printed = Version.print version
        case result of
          Left (FailedDelete err) -> do
            Console.log $ "[ERROR] Failed to delete " <> printed <> ": " <> err
          Left (FailedUpdateMetadata err) -> do
            Console.log $ "[ERROR] Failed to update metadata file for " <> printed <> ": " <> err
          Left (FailedUpdateIndex err) -> do
            Console.log $ "[ERROR] Failed to update index file for " <> printed <> ": " <> err
          Right _ ->
            Console.log $ "Successfully removed " <> printed

    registryDir <- asks _.registry
    commitMetadata <- liftAff $ Except.runExceptT do
      GitHubToken token <- Git.configurePacchettiBotti (Just registryDir)
      Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just registryDir)
      Git.runGit_ [ "add", "*.json" ] (Just registryDir)
      Git.runGit_ [ "commit", "-m", "Remove some package versions from metadata." ] (Just registryDir)
      let upstreamRepo = Constants.registry.owner <> "/" <> Constants.registry.repo
      let origin = "https://pacchettibotti:" <> token <> "@github.com/" <> upstreamRepo <> ".git"
      void $ Git.runGitSilent [ "push", origin, "main" ] (Just registryDir)

    case commitMetadata of
      Left err -> Console.log $ "Failed to commit metadata!\n" <> err
      Right _ -> pure unit

    registryIndexDir <- asks _.registryIndex
    commitIndex <- liftAff $ Except.runExceptT do
      GitHubToken token <- Git.configurePacchettiBotti (Just registryIndexDir)
      Git.runGit_ [ "pull", "--rebase", "--autostash" ] (Just registryIndexDir)
      Git.runGit_ [ "add", "." ] (Just registryIndexDir)
      Git.runGit_ [ "commit", "-m", "Remove some package versions from index." ] (Just registryIndexDir)
      let upstreamRepo = Constants.manifestIndex.owner <> "/" <> Constants.manifestIndex.repo
      let origin = "https://pacchettibotti:" <> token <> "@github.com/" <> upstreamRepo <> ".git"
      void $ Git.runGitSilent [ "push", origin, "main" ] (Just registryIndexDir)

    case commitIndex of
      Left err -> Console.log $ "Failed to commit index!\n" <> err
      Right _ -> pure unit

    Console.log "Finished."
-}

-- todo: interpret this, making sure to do a batch thing
deleteVersion :: PackageName -> Version -> Run (REGISTRY + STORAGE + LOG + LOG_EXCEPT ()) Unit
deleteVersion name version = do
  let formatted = formatPackageVersion name version
  Log.info $ "Deleting " <> formatted
  Storage.deleteTarball name version
  Log.info $ "Updating metadata for " <> formatted
  Registry.readMetadata name >>= case _ of
    Nothing -> Log.exit $ "Could not update metadata for " <> formatted <> " becaues no existing metadata was found."
    Just (Metadata old) -> do
      let new = Metadata $ old { published = Map.delete version old.published, unpublished = Map.delete version old.unpublished }
      Registry.writeMetadata name new
      Registry.deleteManifest name version
