module Registry.Scripts.PackageDeleter where

import Registry.App.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Control.Monad.Except as Except
import Control.Monad.Reader (ask, asks)
import Data.Array as Array
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Data.String as String
import Data.Tuple (uncurry)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Ref as Ref
import Node.Path as Path
import Node.Process as Process
import Registry.App.API as API
import Registry.App.CLI.Git as Git
import Registry.App.Cache as Cache
import Registry.App.GitHub (GitHubToken(..))
import Registry.App.GitHub as GitHub
import Registry.App.Json as Json
import Registry.App.PackageStorage as PackageStorage
import Registry.App.RegistryM (Env, RegistryM, readPackagesMetadata, runRegistryM)
import Registry.Constants as Constants
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Internal.Codec as Internal.Codec
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Version as Version
import Test.Utils (formatPackageVersion)

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

  _ <- API.loadEnv
  FS.Extra.ensureDirectory API.scratchDir

  octokit <- liftEffect do
    token <- do
      result <- Process.lookupEnv "PACCHETTIBOTTI_TOKEN"
      maybe (Exception.throw "PACCHETTIBOTTI_TOKEN not defined in the environment.") (pure <<< GitHubToken) result
    GitHub.newOctokit token

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
      File path -> liftAff (Json.readJsonFile deletePackagesCodec path) >>= case _ of
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

    Console.log "Finished."

data DeleteError
  = FailedDelete String
  | FailedUpdateMetadata String
  | FailedUpdateIndex String

deleteVersion :: PackageName -> Version -> RegistryM (Either DeleteError Unit)
deleteVersion name version = do
  let formatted = formatPackageVersion name version
  Console.log $ "Updating metadata for " <> formatted
  allMetadata <- readPackagesMetadata
  case Map.lookup name allMetadata of
    Nothing -> pure $ Left $ FailedUpdateMetadata "No existing metadata found."
    Just (Metadata oldMetadata) -> do
      let
        publishment = case Map.lookup version oldMetadata.published, Map.lookup version oldMetadata.unpublished of
          Just _, Just _ -> unsafeCrashWith $ "Package version was both published and unpublished: " <> formatted
          Just published, Nothing -> Just (Right published)
          Nothing, Just unpublished -> Just (Left unpublished)
          Nothing, Nothing -> Nothing
      let
        newMetadata = Metadata $ oldMetadata
          { published = Map.delete version oldMetadata.published
          , unpublished = Map.delete version oldMetadata.unpublished
          }
      registryDir <- asks _.registry
      liftAff (Aff.attempt (Json.writeJsonFile Metadata.codec (API.metadataFile registryDir name) newMetadata)) >>= case _ of
        Left err -> pure $ Left $ FailedUpdateMetadata $ Aff.message err
        Right _ -> do
          Console.log $ "Updating manifest index for " <> formatted
          registryIndexDir <- asks _.registryIndex
          liftAff (ManifestIndex.removeFromEntryFile registryIndexDir name version) >>= case _ of
            Left err -> pure $ Left $ FailedUpdateIndex err
            Right _ -> Right unit <$ case publishment of
              Nothing -> Console.log "Why were you deleting a never published package version?"
              Just (Left _) -> Console.log "Why were you deleting an unpublished package version?"
              Just (Right manifest) -> do
                API.publish API.Importer
                  { location: Just oldMetadata.location
                  , name: name
                  , ref: manifest.ref
                  , compiler: unsafeFromRight $ Version.parse "0.15.4"
                  , resolutions: Nothing
                  } newMetadata
