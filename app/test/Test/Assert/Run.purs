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
import Data.Map as Map
import Data.Set as Set
import Effect.Aff as Aff
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
import Registry.Foreign.Octokit (IssueNumber(..))
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
  ( PURSUIT + REGISTRY + PACKAGE_SETS + STORAGE + GITHUB + PACCHETTIBOTTI_ENV + GITHUB_EVENT_ENV + GITHUB_CACHE + LEGACY_CACHE + LOG
      -- Run-provided effects
      + EXCEPT String
      + AFF
      + EFFECT
      + ()
  )

type TestEnv =
  { metadata :: Map PackageName Metadata
  , index :: ManifestIndex
  , username :: String
  }

readFixtures :: Aff { metadata :: Map PackageName Metadata, index :: ManifestIndex }
readFixtures = do
  let
    run = do
      metadata <- Registry.readAllMetadataFromDisk metadataFixtures
      index <- Registry.readManifestIndexFromDisk manifestFixtures
      pure { metadata, index }

  run
    # Log.interpret (\(Log _ _ next) -> pure next)
    # Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
    # Run.runBaseAff'

-- FIXME: Rename to 'runTestEffects'
runTestEffects :: forall a. TestEnv -> Run TEST_EFFECTS a -> Aff (Either String a)
runTestEffects env =
  Pursuit.interpret handlePursuitMock
    >>> Registry.interpret (handleRegistryMock env.metadata env.index)
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

metadataFixtures :: FilePath
metadataFixtures = Path.concat [ "test", "_fixtures", "registry-metadata" ]

manifestFixtures :: FilePath
manifestFixtures = Path.concat [ "test", "_fixtures", "registry-index" ]

handlePursuitMock :: forall r a. Pursuit a -> Run r a
handlePursuitMock = case _ of
  Publish _json reply ->
    pure $ reply $ Right unit
  GetPublishedVersions _name reply ->
    -- FIXME: Reply with a map of versions based on the fixtures.
    pure $ reply $ Right $ Map.empty

handleRegistryMock :: forall r a. Map PackageName Metadata -> ManifestIndex -> Registry a -> Run r a
handleRegistryMock metadata index = case _ of
  ReadManifest name version reply ->
    pure $ reply $ Right $ ManifestIndex.lookup name version index
  WriteManifest _manifest reply ->
    -- FIXME: Actually write the manifest (however the test is set up for that)
    pure $ reply $ Right unit
  DeleteManifest _name _version reply ->
    -- FIXME: Actually delete the manifest (however the test is set up for that)
    pure $ reply $ Right unit
  ReadAllManifests reply ->
    pure $ reply $ Right index
  ReadMetadata name reply ->
    pure $ reply $ Right $ Map.lookup name metadata
  WriteMetadata _name _metadata reply ->
    -- FIXME: Actually write the metadata
    pure $ reply $ Right unit
  ReadAllMetadata reply ->
    pure $ reply $ Right metadata
  ReadLatestPackageSet reply ->
    -- FIXME: Actually reply with a package set
    pure $ reply $ Right Nothing
  WritePackageSet _packageSet _message reply ->
    -- FIXME: Actually write package set
    pure $ reply $ Right unit
  ReadAllPackageSets reply ->
    -- FIXME: Actually reply with a package set
    pure $ reply $ Right Map.empty

  -- Legacy operations
  MirrorPackageSet _packageSet reply ->
    pure $ reply $ Right unit
  ReadLegacyRegistry reply ->
    pure $ reply $ Right { bower: Map.empty, new: Map.empty }
  MirrorLegacyRegistry _name _location reply ->
    pure $ reply $ Right unit

handlePackageSetsMock :: forall r a. PackageSets a -> Run r a
handlePackageSetsMock = case _ of
  UpgradeAtomic _packageSet _compilerVersion _changeSet reply -> do
    -- FIXME: Actually reply with a package set with a pure upgrade
    pure $ reply $ Right Nothing
  UpgradeSequential packageSet _compilerVersion changeSet reply ->
    -- FIXME: Actually reply with a package sequential upgrade result
    pure $ reply $ Right $ Just { failed: changeSet, succeeded: changeSet, result: packageSet }

handleStorageMock :: forall r a. Storage a -> Run (AFF + r) a
handleStorageMock = case _ of
  Storage.Upload _name _version _destinationPath reply ->
    pure $ reply $ Right unit
  Storage.Download name version destinationPath reply ->
    -- FIXME: We probably shouldn't be storing tarballs in fixtures.
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
  Storage.Query _name reply -> do
    -- FIXME: Actually reply with a set of versions based on the fixtures.
    pure $ reply $ Right Set.empty

handleGitHubMock :: forall r a. GitHub a -> Run r a
handleGitHubMock = case _ of
  ListTags _address reply ->
    -- FIXME: Respond with an actual list of tags corresponding with the repo?
    pure $ reply $ Right []
  ListTeamMembers _team reply ->
    -- FIXME: Respond with an actual list of team members if the team is the purescript owners (error otherwise)?
    pure $ reply $ Right []
  GetContent _address _ref _path reply ->
    -- FIXME: Respond with a  file for specific input paths?
    -- Take it from fixtures?
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
  GetRefCommit _address _ref reply ->
    -- FIXME: Respond with an actual commit for specific input paths?
    pure $ reply $ Right "Unimplemented"
  GetCommitDate _address _ref reply ->
    -- FIXME: Respond with an actual datetime for specific inputs?
    pure $ reply $ Right top

runLegacyCacheMemory :: forall r a. CacheRef -> Run (LEGACY_CACHE + LOG + EFFECT + r) a -> Run (LOG + EFFECT + r) a
runLegacyCacheMemory = Cache.interpret Legacy.Manifest._legacyCache <<< Cache.handleMemory

runGitHubCacheMemory :: forall r a. CacheRef -> Run (GITHUB_CACHE + LOG + EFFECT + r) a -> Run (LOG + EFFECT + r) a
runGitHubCacheMemory = Cache.interpret GitHub._githubCache <<< Cache.handleMemory
