-- | Helper code for running tests in Run, including mock implementations of
-- | the various registry effects and fixtures for a minimal registry.
module Registry.Test.Assert.Run
  ( TEST_EFFECTS
  , runTestEffects
  , readFixtures
  , shouldContain
  , shouldNotContain
  ) where

import Registry.App.Prelude

import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Set as Set
import Effect.Aff as Aff
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Node.Path as Path
import Registry.App.Effect.Cache (CacheRef)
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env (GITHUB_EVENT_ENV, PACCHETTIBOTTI_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.GitHub (GITHUB, GITHUB_CACHE, GitHub(..))
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG, Log(..))
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets (PACKAGE_SETS, PackageSets(..))
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Pursuit (PURSUIT, Pursuit(..))
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry (REGISTRY, Registry(..))
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage (STORAGE, Storage)
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.Manifest (LEGACY_CACHE)
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit (GitHubError(..), IssueNumber(..))
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Test.Utils as Utils
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

-- | The standard `shouldContain` assertion, suitable for use with Run
shouldContain :: forall f a r. Eq a => Foldable f => f a -> a -> Run (EXCEPT String + r) Unit
shouldContain container elem =
  when (elem `Foldable.notElem` container) do
    Except.throw (Utils.unsafeStringify elem <> "\n\nshould be a member of\n\n" <> Utils.unsafeStringify container)

-- | The standard `shouldNotContain` assertion, suitable for use with Run
shouldNotContain :: forall f a r. Eq a => Foldable f => f a -> a -> Run (EXCEPT String + r) Unit
shouldNotContain container elem =
  unless (elem `Foldable.notElem` container) do
    Except.throw (Utils.unsafeStringify elem <> "\n\nis, but should not be, a member of\n\n" <> Utils.unsafeStringify container)

-- | All effects possible when testing the registry API (not all operations use
-- | all effects, but this union is the maximum set of effects that can be used.)
type TEST_EFFECTS =
  ( PURSUIT
      + REGISTRY
      + PACKAGE_SETS
      + STORAGE
      + GITHUB
      + PACCHETTIBOTTI_ENV
      + GITHUB_EVENT_ENV
      + GITHUB_CACHE
      + LEGACY_CACHE
      + LOG
      + EXCEPT String
      + AFF
      + EFFECT
      + ()
  )

type TestEnv =
  { metadata :: Ref (Map PackageName Metadata)
  , index :: Ref ManifestIndex
  , username :: String
  }

readFixtures :: Aff { metadata :: Ref (Map PackageName Metadata), index :: Ref ManifestIndex }
readFixtures = do
  let
    run = do
      initialMetadata <- Registry.readAllMetadataFromDisk metadataFixtures
      metadata <- liftEffect $ Ref.new initialMetadata

      initialIndex <- Registry.readManifestIndexFromDisk manifestFixtures
      index <- liftEffect $ Ref.new initialIndex

      pure { metadata, index }

  run
    # Log.interpret (\(Log _ _ next) -> pure next)
    # Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
    # Run.runBaseAff'

-- FIXME: Rename to 'runTestEffects'
runTestEffects :: forall a. TestEnv -> Run TEST_EFFECTS a -> Aff (Either String a)
runTestEffects env =
  Pursuit.interpret (handlePursuitMock env.metadata)
    >>> Registry.interpret (handleRegistryMock { metadataRef: env.metadata, indexRef: env.index })
    >>> PackageSets.interpret handlePackageSetsMock
    >>> Storage.interpret handleStorageMock
    >>> GitHub.interpret handleGitHubMock
    -- Environments
    >>> Env.runGitHubEventEnv { username: env.username, issue: IssueNumber 1 }
    >>> Env.runPacchettiBottiEnv { publicKey: "Unimplemented", privateKey: "Unimplemented" }
    -- Caches
    >>> runGitHubCacheMemory (unsafePerformEffect Cache.newCacheRef)
    >>> runLegacyCacheMemory (unsafePerformEffect Cache.newCacheRef)
    -- Other effects
    >>> Log.interpret (\(Log _ _ next) -> pure next)
    -- Base effects
    >>> Except.runExcept
    >>> Run.runBaseAff'

runLegacyCacheMemory :: forall r a. CacheRef -> Run (LEGACY_CACHE + LOG + EFFECT + r) a -> Run (LOG + EFFECT + r) a
runLegacyCacheMemory = Cache.interpret Legacy.Manifest._legacyCache <<< Cache.handleMemory

runGitHubCacheMemory :: forall r a. CacheRef -> Run (GITHUB_CACHE + LOG + EFFECT + r) a -> Run (LOG + EFFECT + r) a
runGitHubCacheMemory = Cache.interpret GitHub._githubCache <<< Cache.handleMemory

metadataFixtures :: FilePath
metadataFixtures = Path.concat [ "test", "_fixtures", "registry-metadata" ]

manifestFixtures :: FilePath
manifestFixtures = Path.concat [ "test", "_fixtures", "registry-index" ]

handlePursuitMock :: forall r a. Ref (Map PackageName Metadata) -> Pursuit a -> Run (EFFECT + r) a
handlePursuitMock metadataRef = case _ of
  Publish _json reply ->
    pure $ reply $ Right unit
  GetPublishedVersions name reply -> do
    metadata <- Run.liftEffect (Ref.read metadataRef)
    pure $ reply $ Right $ fromMaybe Map.empty do
      Metadata { published } <- Map.lookup name metadata
      pure $ mapWithIndex (\version _ -> "https://pursuit.purescript.org/purescript-" <> PackageName.print name <> "/" <> Version.print version) published

type RegistryMockEnv =
  { metadataRef :: Ref (Map PackageName Metadata)
  , indexRef :: Ref ManifestIndex
  }

handleRegistryMock :: forall r a. RegistryMockEnv -> Registry a -> Run (AFF + EFFECT + r) a
handleRegistryMock env = case _ of
  ReadManifest name version reply -> do
    index <- Run.liftEffect (Ref.read env.indexRef)
    pure $ reply $ Right $ ManifestIndex.lookup name version index
  WriteManifest manifest reply -> do
    index <- Run.liftEffect (Ref.read env.indexRef)
    case ManifestIndex.insert manifest index of
      Left err -> pure $ reply $ Left $ "Failed to insert manifest:\n" <> Utils.unsafeStringify manifest <> " due to an error:\n" <> Utils.unsafeStringify err
      Right index' -> do
        Run.liftEffect (Ref.write index' env.indexRef)
        pure $ reply $ Right unit
  DeleteManifest name version reply -> do
    index <- Run.liftEffect (Ref.read env.indexRef)
    case ManifestIndex.delete name version index of
      Left err -> pure $ reply $ Left $ "Failed to delete entry for :\n" <> Utils.formatPackageVersion name version <> " due to an error:\n" <> Utils.unsafeStringify err
      Right index' -> do
        Run.liftEffect (Ref.write index' env.indexRef)
        pure $ reply $ Right unit
  ReadAllManifests reply -> do
    index <- Run.liftEffect (Ref.read env.indexRef)
    pure $ reply $ Right index
  ReadMetadata name reply -> do
    metadata <- Run.liftEffect (Ref.read env.metadataRef)
    pure $ reply $ Right $ Map.lookup name metadata
  WriteMetadata name metadata reply -> do
    Run.liftEffect (Ref.modify_ (Map.insert name metadata) env.metadataRef)
    pure $ reply $ Right unit
  ReadAllMetadata reply -> do
    metadata <- Run.liftEffect (Ref.read env.metadataRef)
    pure $ reply $ Right metadata

  -- FIXME: Actually reply with a package set
  ReadLatestPackageSet reply ->
    pure $ reply $ Right Nothing
  -- FIXME: Actually write package set
  WritePackageSet _packageSet _message reply ->
    pure $ reply $ Right unit
  -- FIXME: Actually reply with a package set
  ReadAllPackageSets reply ->
    pure $ reply $ Right Map.empty

  -- Legacy operations; we just treat these as successful by default.
  MirrorPackageSet _packageSet reply ->
    pure $ reply $ Right unit
  ReadLegacyRegistry reply ->
    pure $ reply $ Right { bower: Map.empty, new: Map.empty }
  MirrorLegacyRegistry _name _location reply ->
    pure $ reply $ Right unit

handlePackageSetsMock :: forall r a. PackageSets a -> Run r a
handlePackageSetsMock = case _ of
  -- FIXME: Actually reply with a package set with a pure upgrade
  UpgradeAtomic _packageSet _compilerVersion _changeSet reply -> do
    pure $ reply $ Right Nothing
  -- FIXME: Actually reply with a package sequential upgrade result
  UpgradeSequential packageSet _compilerVersion changeSet reply ->
    pure $ reply $ Right $ Just { failed: changeSet, succeeded: changeSet, result: packageSet }

handleStorageMock :: forall r a. Storage a -> Run (AFF + r) a
handleStorageMock = case _ of
  Storage.Upload _name _version _destinationPath reply ->
    pure $ reply $ Right unit

  -- FIXME: We probably shouldn't be storing tarballs in fixtures.
  Storage.Download name version destinationPath reply ->
    if name == Utils.unsafePackageName "prelude" && version == Utils.unsafeVersion "6.0.1" then do
      -- Note: When we're inside the test effects we are running from the root
      -- of the repository, so we need "app" at the start of the path.
      let sourcePath = Path.concat [ "app", "test", "_fixtures", "registry-storage", "prelude-6.0.1.tar.gz" ]
      Run.liftAff $ FS.Extra.copy { from: sourcePath, to: destinationPath, preserveTimestamps: false }
      pure $ reply $ Right unit
    else
      pure $ reply $ Left $ "No such fixture: " <> PackageName.print name <> " " <> Version.print version

  Storage.Delete _name _version reply ->
    pure $ reply $ Right unit

  -- FIXME: Actually reply with a set of versions based on the fixtures.
  Storage.Query _name reply -> do
    pure $ reply $ Right Set.empty

handleGitHubMock :: forall r a. GitHub a -> Run r a
handleGitHubMock = case _ of
  -- FIXME: Respond with an actual list of tags corresponding with the repo?
  ListTags _address reply ->
    pure $ reply $ Right []

  -- FIXME: Respond with an actual list of team members if the team is the purescript owners (error otherwise)?
  ListTeamMembers _team reply ->
    pure $ reply $ Right []

  -- FIXME: Read from the local package in fixtures.
  GetContent address ref path reply ->
    if address == { owner: "purescript", repo: "purescript-effect" } && ref == "v4.0.0" && path == "bower.json" then
      pure $ reply $ Right
        """
        {
          "name": "purescript-effect",
          "license": "MIT",
          "dependencies": {
            "purescript-prelude": "^6.0.0"
          }
        }
        """
    else
      pure $ reply $ Left $ APIError { statusCode: 404, message: "Not Found" }

  -- FIXME: Respond with an actual commit for specific input paths?
  GetRefCommit _address _ref reply ->
    pure $ reply $ Right "Unimplemented"

  -- FIXME: Respond with an actual datetime for specific inputs?
  GetCommitDate _address _ref reply ->
    pure $ reply $ Right top
