-- | A test script that exercises publishing to Pursuit only.
module Test.Scripts.PublishPursuit where

import Registry.App.Prelude

import Effect.Exception (throw)
import Foreign.Git as Git
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Node.Path as Path
import Node.Process as Process
import Record as Record
import Registry.App.API (Source(..))
import Registry.App.API as API
import Registry.App.Cache as Cache
import Registry.App.Json as Json
import Registry.App.Monad (LocalEnv)
import Registry.App.Monad as App
import Registry.Effect.Log (LogVerbosity(..))
import Registry.Effect.Log as Log
import Registry.Effect.Notify as Notify
import Registry.Manifest as Manifest
import Registry.Version as Version
import Test.TestM as TestM

main :: Effect Unit
main = launchAff_ $ do
  _ <- API.loadEnv

  githubToken <- liftEffect do
    Process.lookupEnv "GITHUB_TOKEN"
      >>= maybe (throw "GITHUB_TOKEN not defined in the environment") (pure <<< GitHubToken)

  octokit <- liftEffect $ GitHub.mkOctokit githubToken
  cache <- Cache.useCache API.cacheDir

  let
    env :: LocalEnv
    env =
      TestM.defaultTestEnv
        { registryIndex = Path.concat [ "..", "scratch", "registry-index" ]
        , octokit = octokit
        , cache = cache
        } # Record.merge { verbosity: Verbose, logfile: Path.concat [ "..", "scratch", "test-publish-pursuit-log.txt" ] }

  App.runLocalM env do
    tmp <- liftEffect Tmp.mkTmpDir
    API.fetchRegistryIndex
    liftAff $ Git.cloneGitTag ("https://github.com/thomashoneyman/purescript-slug") "v3.0.6" tmp

    let
      packageSourceDir = Path.concat [ tmp, "purescript-slug" ]
      eitherManifest = Json.parseJson Manifest.codec """{"name":"slug","version":"3.0.6","license":"MIT","location":{"githubOwner":"thomashoneyman","githubRepo":"purescript-slug"},"dependencies":{"argonaut-codecs":">=9.0.0 <10.0.0","arrays":">=7.0.0 <8.0.0","either":">=6.1.0 <7.0.0","maybe":">=6.0.0 <7.0.0","prelude":">=6.0.0 <7.0.0","strings":">=6.0.0 <7.0.0","unicode":">=6.0.0 <7.0.0"}}"""
      compiler = unsafeFromRight $ Version.parse "0.15.4"

    case eitherManifest of
      Left err ->
        Notify.exit err
      Right manifest -> do
        liftAff $ Json.writeJsonFile Manifest.codec (Path.concat [ packageSourceDir, "purs.json" ]) manifest
        API.verifyResolutions { source: API, resolutions: Nothing, manifest } >>= case _ of
          Left err -> Notify.exit err
          Right verified -> do
            API.compilePackage { packageSourceDir, resolutions: verified, compiler } >>= case _ of
              Left err -> Notify.exit err
              Right dependenciesDir -> do
                files <- liftAff $ FS.readdir packageSourceDir
                Log.debug $ show files
                deps <- liftAff $ FS.readdir dependenciesDir
                Log.debug $ show deps
                result <- API.publishToPursuit { packageSourceDir, compiler, resolutions: verified, dependenciesDir }
                case result of
                  Left error -> Notify.exit error
                  Right message -> Log.debug message
