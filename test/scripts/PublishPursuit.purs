-- | A test script that exercises publishing to Pursuit only.
module PublishPursuit where

import Registry.Prelude

import Data.Map as Map
import Dotenv as Dotenv
import Effect.Exception (throw)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.GitHub (GitHubToken(..))
import Foreign.GitHub as GitHub
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Node.Path as Path
import Node.Process as Process
import Registry.API (cloneGitTag, publishToPursuit)
import Registry.Cache as Cache
import Registry.PackageName as PackageName
import Registry.RegistryM (Env, runRegistryM)
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
      , registryIndex: mempty
      }

  runRegistryM env do
    tmpDir <- liftEffect Tmp.mkTmpDir
    liftAff $ cloneGitTag ("https://github.com/purescript/purescript-console") "v5.0.0" tmpDir
    let
      packageSourceDir = tmpDir <> Path.sep <> "purescript-console"
      pursJson =
        """
        {
          "name": "console",
          "version": "5.0.0",
          "license": "BSD-3-Clause",
          "location": {
            "githubOwner": "purescript",
            "githubRepo": "purescript-console"
          },
          "dependencies": {
            "effect": ">=3.0.0 <4.0.0",
            "prelude": ">=5.0.0 <6.0.0"
          }
        }
        """

      buildPlan = BuildPlan
        { compiler: unsafeFromRight (Version.parseVersion Version.Lenient "v0.14.7")
        , resolutions: Map.fromFoldable
            [ Tuple (unsafeFromRight (PackageName.parse "prelude")) (unsafeFromRight (Version.parseVersion Version.Lenient "v5.0.0"))
            , Tuple (unsafeFromRight (PackageName.parse "effect")) (unsafeFromRight (Version.parseVersion Version.Lenient "v3.0.0"))
            ]
        }

    liftAff $ FS.writeTextFile UTF8 (packageSourceDir <> Path.sep <> "purs.json") pursJson

    publishToPursuit
      { packageSourceDir
      , buildPlan
      }
