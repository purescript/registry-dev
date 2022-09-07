-- | A test script that exercises publishing to Pursuit only.
module PublishPursuit where

import Registry.Prelude

import Data.Map as Map
import Dotenv as Dotenv
import Effect.Exception (throw)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Git as Git
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Node.Path as Path
import Node.Process as Process
import Registry.API (Source(..), compilePackage, publishToPursuit, verifyBuildPlan)
import Registry.Cache as Cache
import Registry.Json as Json
import Registry.RegistryM (Env, runRegistryM, throwWithComment)
import Registry.Schema (BuildPlan(..))
import Registry.Version as Version

main :: Effect Unit
main = launchAff_ $ do
  _ <- Dotenv.loadFile

  githubToken <- liftEffect do
    Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  octokit <- liftEffect $ GitHub.mkOctokit githubToken
  cache <- Cache.useCache

  let
    env :: Env
    env =
      { comment: mempty
      , closeIssue: mempty
      , commitMetadataFile: \_ _ -> pure (Right unit)
      , commitIndexFile: \_ _ -> pure (Right unit)
      , commitPackageSetFile: \_ _ _ -> pure (Right unit)
      , uploadPackage: mempty
      , deletePackage: mempty
      , packagesMetadata: unsafePerformEffect (Ref.new Map.empty)
      , cache
      , octokit
      , username: mempty
      , registry: mempty
      , registryIndex: Path.concat [ "scratch", "registry-index" ]
      }

  runRegistryM env do
    tmp <- liftEffect Tmp.mkTmpDir
    liftAff $ Git.cloneGitTag ("https://github.com/thomashoneyman/purescript-slug") "v3.0.6" tmp

    let
      packageSourceDir = Path.concat [ tmp, "purescript-slug" ]
      eitherManifest = Json.parseJson """{"name":"slug","version":"3.0.6","license":"MIT","location":{"githubOwner":"thomashoneyman","githubRepo":"purescript-slug"},"dependencies":{"argonaut-codecs":">=9.0.0 <10.0.0","arrays":">=7.0.0 <8.0.0","either":">=6.1.0 <7.0.0","maybe":">=6.0.0 <7.0.0","prelude":">=6.0.0 <7.0.0","strings":">=6.0.0 <7.0.0","unicode":">=6.0.0 <7.0.0"}}"""
      providedBuildPlan = BuildPlan
        { compiler: unsafeFromRight (Version.parseVersion Version.Lenient "v0.15.4")
        , resolutions: Nothing
        }

    case eitherManifest of
      Left err ->
        throwWithComment err
      Right manifest -> do
        liftAff $ Json.writeJsonFile (Path.concat [ packageSourceDir, "purs.json" ]) manifest
        buildPlan <- verifyBuildPlan { source: API, buildPlan: providedBuildPlan, manifest }
        compilePackage { packageSourceDir, buildPlan } >>= case _ of
          Left err -> throwWithComment err
          Right dependenciesDir -> do
            files <- liftAff $ FS.readdir packageSourceDir
            logShow files
            deps <- liftAff $ FS.readdir dependenciesDir
            logShow deps
            publishToPursuit { packageSourceDir, buildPlan, dependenciesDir }
