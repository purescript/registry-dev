-- | A test script that exercises publishing to Pursuit only.
module Test.Scripts.PublishPursuit where

import Registry.Prelude

import Data.Map as Map
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
import Registry.API (Source(..), compilePackage, publishToPursuit)
import Registry.API as API
import Registry.App.Json as Json
import Registry.Cache as Cache
import Registry.Manifest as Manifest
import Registry.RegistryM (Env, runRegistryM, throwWithComment)
import Registry.Version as Version

main :: Effect Unit
main = launchAff_ $ do
  _ <- API.loadEnv

  githubToken <- liftEffect do
    Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  octokit <- liftEffect $ GitHub.mkOctokit githubToken
  cache <- Cache.useCache API.cacheDir

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
      , registryIndex: Path.concat [ "..", "scratch", "registry-index" ]
      }

  runRegistryM env do
    tmp <- liftEffect Tmp.mkTmpDir
    API.fetchRegistryIndex
    liftAff $ Git.cloneGitTag ("https://github.com/thomashoneyman/purescript-slug") "v3.0.6" tmp

    let
      packageSourceDir = Path.concat [ tmp, "purescript-slug" ]
      eitherManifest = Json.parseJson Manifest.codec """{"name":"slug","version":"3.0.6","license":"MIT","location":{"githubOwner":"thomashoneyman","githubRepo":"purescript-slug"},"dependencies":{"argonaut-codecs":">=9.0.0 <10.0.0","arrays":">=7.0.0 <8.0.0","either":">=6.1.0 <7.0.0","maybe":">=6.0.0 <7.0.0","prelude":">=6.0.0 <7.0.0","strings":">=6.0.0 <7.0.0","unicode":">=6.0.0 <7.0.0"}}"""
      compiler = unsafeFromRight $ Version.parse "0.15.4"

    case eitherManifest of
      Left err ->
        throwWithComment err
      Right manifest -> do
        liftAff $ Json.writeJsonFile Manifest.codec (Path.concat [ packageSourceDir, "purs.json" ]) manifest
        API.verifyResolutions { source: API, resolutions: Nothing, manifest } >>= case _ of
          Left err -> throwWithComment err
          Right verified -> do
            compilePackage { packageSourceDir, resolutions: verified, compiler } >>= case _ of
              Left err -> throwWithComment err
              Right dependenciesDir -> do
                files <- liftAff $ FS.readdir packageSourceDir
                logShow files
                deps <- liftAff $ FS.readdir dependenciesDir
                logShow deps
                result <- publishToPursuit { packageSourceDir, compiler, resolutions: verified, dependenciesDir }
                case result of
                  Left error -> throwWithComment error
                  Right message -> log message
