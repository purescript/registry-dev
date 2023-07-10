-- | Helper code for running tests in Run, including mock implementations of
-- | the various registry effects and fixtures for a minimal registry.
module Registry.Test.Assert.Run
  ( TEST_EFFECTS
  , runBaseEffects
  , runTestEffects
  , shouldContain
  , shouldNotContain
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Debug (traceM)
import Effect.Aff as Aff
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Node.FS.Aff as FS.Aff
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
import Registry.App.Prelude as Either
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
  { workdir :: FilePath
  , metadata :: Ref (Map PackageName Metadata)
  , index :: Ref ManifestIndex
  , storage :: FilePath
  , username :: String
  }

-- FIXME: Rename to 'runTestEffects'
runTestEffects :: forall a. TestEnv -> Run TEST_EFFECTS a -> Aff a
runTestEffects env =
  Pursuit.interpret (handlePursuitMock env.metadata)
    >>> Registry.interpret (handleRegistryMock { metadataRef: env.metadata, indexRef: env.index })
    >>> PackageSets.interpret handlePackageSetsMock
    >>> Storage.interpret (handleStorageMock { storage: env.storage })
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
    >>> Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
    >>> Run.runBaseAff'

-- | For testing simple Run functions that don't need the whole environment.
runBaseEffects :: forall a. Run (LOG + EXCEPT String + AFF + EFFECT + ()) a -> Aff a
runBaseEffects =
  Log.interpret (\(Log _ _ next) -> pure next)
    -- Base effects
    >>> Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
    >>> Run.runBaseAff'

runLegacyCacheMemory :: forall r a. CacheRef -> Run (LEGACY_CACHE + LOG + EFFECT + r) a -> Run (LOG + EFFECT + r) a
runLegacyCacheMemory = Cache.interpret Legacy.Manifest._legacyCache <<< Cache.handleMemory

runGitHubCacheMemory :: forall r a. CacheRef -> Run (GITHUB_CACHE + LOG + EFFECT + r) a -> Run (LOG + EFFECT + r) a
runGitHubCacheMemory = Cache.interpret GitHub._githubCache <<< Cache.handleMemory

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

type StorageMockEnv = { storage :: FilePath }

-- We handle the storage effect by copying files to/from the provided
-- upload/download directories, and listing versions based on the filenames.
handleStorageMock :: forall r a. StorageMockEnv -> Storage a -> Run (AFF + r) a
handleStorageMock env = case _ of
  Storage.Upload name version sourcePath reply -> do
    let destinationPath = Path.concat [ env.storage, PackageName.print name <> "-" <> Version.print version <> ".tar.gz" ]
    Run.liftAff (Aff.attempt (FS.Aff.stat destinationPath)) >>= case _ of
      Left _ -> do
        Run.liftAff $ FS.Extra.copy { from: sourcePath, to: destinationPath, preserveTimestamps: true }
        pure $ reply $ Right unit
      Right _ ->
        pure $ reply $ Left $ "Cannot upload " <> formatPackageVersion name version <> " because it already exists in storage at path " <> destinationPath

  Storage.Download name version destinationPath reply -> do
    let sourcePath = Path.concat [ env.storage, PackageName.print name <> "-" <> Version.print version <> ".tar.gz" ]
    Run.liftAff $ FS.Extra.copy { from: sourcePath, to: destinationPath, preserveTimestamps: true }
    pure $ reply $ Right unit

  Storage.Delete name version reply -> do
    let sourcePath = Path.concat [ env.storage, PackageName.print name <> "-" <> Version.print version <> ".tar.gz" ]
    Run.liftAff (Aff.attempt (FS.Aff.stat sourcePath)) >>= case _ of
      Left _ -> pure $ reply $ Left $ "Cannot delete " <> sourcePath <> " because it does not exist in download directory."
      Right _ -> do
        Run.liftAff $ FS.Extra.remove sourcePath
        pure $ reply $ Right unit

  Storage.Query name reply -> do
    paths <- Run.liftAff $ FS.Aff.readdir env.storage

    let
      extractVersion =
        String.stripPrefix (String.Pattern (PackageName.print name <> "-"))
          >=> String.stripSuffix (String.Pattern ".tar.gz")

      versions = Array.mapMaybe (Either.hush <<< Version.parse <=< extractVersion) paths

    pure $ reply $ Right $ Set.fromFoldable versions

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
