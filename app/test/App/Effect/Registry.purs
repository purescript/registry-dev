module Test.Registry.App.Effect.Registry (spec) where

import Registry.App.Prelude

import Data.Map as Map
import Effect.Aff as Aff
import Effect.Ref as Ref
import Node.Path as Path
import Registry.API.V1 (JobId(..))
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Log (LOG, Log(..))
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Registry (REGISTRY_READ, RegistryEnv, WriteMode(..))
import Registry.App.Effect.Registry as Registry
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Tmp as Tmp
import Registry.Metadata (Metadata(..))
import Registry.Metadata as Metadata
import Registry.Test.Assert as Assert
import Registry.Test.Fixtures (defaultHash, defaultLocation)
import Registry.Test.Utils (unsafeDateTime, unsafeNonEmptyArray, unsafePackageName, unsafeVersion)
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.it "appends the executing job ID to commit messages" do
    let message = "Update metadata for prelude"
    Registry.appendJobIdToCommitMessage Nothing message `Assert.shouldEqual` message
    Registry.appendJobIdToCommitMessage (Just (JobId "job-123")) message `Assert.shouldEqual`
      "Update metadata for prelude\n\nJob ID: job-123"

  -- This test exercises Registry.handleRead to verify that readMetadata does
  -- not poison the AllMetadata cache: i.e. a single-package read must not seed
  -- the cache with a singleton map that readAllMetadata would mistake for the
  -- complete set.
  Spec.it "readMetadata does not poison AllMetadata cache for readAllMetadata" do
    Aff.bracket Tmp.mkTmpDir FS.Extra.remove \tmp -> do
      let metadataDir = Path.concat [ tmp, "registry", "metadata" ]
      FS.Extra.ensureDirectory metadataDir

      -- Write 3 metadata files to disk
      for_ packages \{ name, version, compilers } -> do
        let
          metadata = Metadata
            { location: defaultLocation
            , owners: Nothing
            , published: Map.singleton (unsafeVersion version)
                { bytes: 1000.0
                , compilers: unsafeNonEmptyArray (map unsafeVersion compilers)
                , hash: defaultHash
                , publishedTime: unsafeDateTime "2024-01-01T00:00:00.000Z"
                , ref: Nothing
                }
            , unpublished: Map.empty
            }
        liftAff $ writeJsonFile Metadata.codec (Path.concat [ metadataDir, name <> ".json" ]) metadata

      -- Set up the RegistryEnv with a pre-populated debouncer so pull
      -- returns NoChange without doing any git operations.
      now <- nowUTC
      let registryPath = Path.concat [ tmp, "registry" ]
      debouncer <- liftEffect $ Ref.new (Map.singleton registryPath now)
      cacheRef <- liftEffect Cache.newCacheRef
      let
        env =
          { jobId: Nothing
          , repos:
              { registry: { owner: "test", repo: "test" }
              , manifestIndex: { owner: "test", repo: "test" }
              , legacyPackageSets: { owner: "test", repo: "test" }
              }
          , workdir: tmp
          , pull: Git.ForceClean
          , write: ReadOnly
          , debouncer
          , cacheRef
          }

      -- Step 1: readMetadata for one package.
      -- Before the fix, resetFromDisk seeded the AllMetadata cache with
      -- Map.singleton prelude metadata. After the fix, the cache is left alone.
      _ <- runRealRegistry env $ Registry.readMetadata (unsafePackageName "prelude")

      -- Step 2: readAllMetadata under Git.NoChange.
      -- Before the fix, the singleton cache from step 1 was returned verbatim
      -- and the assertion below would see size 1. After the fix, the handler
      -- reads all three metadata files from disk.
      allMetadata <- runRealRegistry env $ Registry.readAllMetadata

      Map.size allMetadata `Assert.shouldEqual` 3

  where
  packages =
    [ { name: "prelude", version: "6.0.1", compilers: [ "0.15.15" ] }
    , { name: "effect", version: "4.0.0", compilers: [ "0.15.15" ] }
    , { name: "control", version: "6.0.0", compilers: [ "0.15.15" ] }
    ]

  -- | Run the REGISTRY_READ effect - can't use the mock here because the regression
  -- | we are testing is in the caching code of the handle
  runRealRegistry
    :: forall a
     . RegistryEnv
    -> Run (REGISTRY_READ + LOG + EXCEPT String + AFF + EFFECT + ()) a
    -> Aff a
  runRealRegistry env =
    Registry.interpretRead (Registry.handleRead env)
      >>> Log.interpret (\(Log _ _ next) -> pure next)
      >>> Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
      >>> Run.runBaseAff'
