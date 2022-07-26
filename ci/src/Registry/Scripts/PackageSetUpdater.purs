module Registry.Scripts.PackageSetUpdater where

import Registry.Prelude

import Control.Monad.Reader (asks)
import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Dotenv as Dotenv
import Effect.Aff as Aff
import Effect.Exception as Exception
import Effect.Ref as Ref
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Tmp as Tmp
import Node.Process as Node.Process
import Node.Process as Process
import Registry.API as API
import Registry.Index as Index
import Registry.Json as Json
import Registry.PackageName as PackageName
import Registry.PackageSet as PackageSet
import Registry.RegistryM (Env, commitPackageSetFile, runRegistryM, throwWithComment)
import Registry.Schema (PackageSet(..))
import Registry.Version as Version

data PublishMode = GeneratePackageSet | CommitPackageSet

derive instance Eq PublishMode

main :: Effect Unit
main = Aff.launchAff_ do
  _ <- Dotenv.loadFile

  log "Parsing CLI args..."
  mode <- liftEffect do
    args <- Array.drop 2 <$> Node.Process.argv
    case Array.uncons args of
      Nothing -> Exception.throw "Expected 'generate' or 'commit', but received no arguments."
      Just { head, tail: [] } -> case head of
        "generate" -> pure GeneratePackageSet
        "commit" -> pure CommitPackageSet
        other -> Exception.throw $ "Expected 'generate' or 'commit' but received: " <> other
      Just _ -> Exception.throw $ String.joinWith "\n"
        [ "Expected 'generate' or 'commit', but received multiple arguments:"
        , String.joinWith " " args
        ]

  log "Starting package set publishing..."

  githubToken <- liftEffect do
    Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (Exception.throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  octokit <- liftEffect $ GitHub.mkOctokit githubToken

  tmpDir <- liftEffect $ Tmp.mkTmpDir
  liftEffect $ Node.Process.chdir tmpDir

  metadataRef <- liftEffect $ Ref.new Map.empty

  let
    env :: Env
    env =
      { comment: \comment -> log ("[COMMENT] " <> comment)
      , closeIssue: mempty
      , commitMetadataFile: \_ _ -> pure (Right unit)
      , commitIndexFile: \_ _ -> pure (Right unit)
      , commitPackageSetFile: API.pacchettiBottiPushToRegistryPackageSets
      , uploadPackage: mempty
      , deletePackage: mempty
      , octokit
      , cache: { write: mempty, read: \_ -> pure (Left mempty), remove: mempty }
      , username: mempty
      , packagesMetadata: metadataRef
      , registry: "registry"
      , registryIndex: "registry-index"
      }

  runRegistryM env do
    API.fetchRegistryIndex
    API.fetchRegistry
    API.fillMetadataRef

    registryIndexPath <- asks _.registryIndex
    registryIndex <- liftAff $ Index.readRegistryIndex registryIndexPath

    prevPackageSet <- PackageSet.readLatestPackageSet
    candidates <- PackageSet.findPackageSetCandidates registryIndex prevPackageSet

    if Map.isEmpty candidates then do
      log "No new package versions eligible for inclusion in the package set."
    else do
      let logPackage name version = log (PackageName.print name <> "@" <> Version.printVersion version)
      log "Found the following package versions eligible for inclusion in package set:"
      forWithIndex_ candidates logPackage
      PackageSet.processBatch registryIndex prevPackageSet Nothing candidates >>= case _ of
        Nothing -> do
          log "\n----------\nNo packages could be added to the set. All packages failed:"
          forWithIndex_ candidates logPackage
        Just { success, fail, packageSet } -> do
          unless (Map.isEmpty fail) do
            log "\n----------\nSome packages could not be added to the set:"
            forWithIndex_ fail logPackage
          log "\n----------\nNew packages were added to the set!"
          forWithIndex_ success logPackage
          newPath <- PackageSet.getPackageSetPath (un PackageSet packageSet).version
          liftAff $ Json.writeJsonFile newPath packageSet
          case mode of
            GeneratePackageSet -> pure unit
            CommitPackageSet -> commitPackageSetFile packageSet >>= case _ of
              Left err -> throwWithComment $ "Failed to commit package set file: " <> err
              Right _ -> pure unit
