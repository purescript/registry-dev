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
import Data.Exists as Exists
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Dodo as Dodo
import Effect.Aff as Aff
import Effect.Now as Now
import Effect.Ref as Ref
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.API.V1 (LogLevel)
import Registry.App.API (COMPILER_CACHE)
import Registry.App.API as API
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Cache (CacheRef)
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Comment (COMMENT)
import Registry.App.Effect.Comment as Comment
import Registry.App.Effect.Env (GITHUB_EVENT_ENV, PACCHETTIBOTTI_ENV, RESOURCE_ENV)
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
import Registry.App.Effect.Source (SOURCE, Source(..))
import Registry.App.Effect.Source as Source
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
      + SOURCE
      + GITHUB
      + PACCHETTIBOTTI_ENV
      + GITHUB_EVENT_ENV
      + RESOURCE_ENV
      + GITHUB_CACHE
      + LEGACY_CACHE
      + COMPILER_CACHE
      + COMMENT
      + LOG
      + EXCEPT String
      + AFF
      + EFFECT
      + ()
  )

type TestEnv =
  { workdir :: FilePath
  , logs :: Ref (Array (Tuple LogLevel String))
  , metadata :: Ref (Map PackageName Metadata)
  , index :: Ref ManifestIndex
  , pursuitExcludes :: Set PackageName
  , storage :: FilePath
  , github :: FilePath
  , username :: String
  }

runTestEffects :: forall a. TestEnv -> Run TEST_EFFECTS a -> Aff (Either Aff.Error a)
runTestEffects env operation = Aff.attempt do
  resourceEnv <- Env.lookupResourceEnv
  githubCache <- liftEffect Cache.newCacheRef
  legacyCache <- liftEffect Cache.newCacheRef
  operation
    # Pursuit.interpret (handlePursuitMock { metadataRef: env.metadata, excludes: env.pursuitExcludes })
    # Registry.interpret (handleRegistryMock { metadataRef: env.metadata, indexRef: env.index })
    # PackageSets.interpret handlePackageSetsMock
    # Storage.interpret (handleStorageMock { storage: env.storage })
    # Source.interpret (handleSourceMock { github: env.github })
    # GitHub.interpret (handleGitHubMock { github: env.github })
    -- Environments
    # Env.runGitHubEventEnv { username: env.username, issue: IssueNumber 1 }
    # Env.runPacchettiBottiEnv { publicKey: "Unimplemented", privateKey: "Unimplemented" }
    # Env.runResourceEnv resourceEnv
    -- Caches
    # runCompilerCacheMock
    # runGitHubCacheMemory githubCache
    # runLegacyCacheMemory legacyCache
    -- Other effects
    # Comment.interpret Comment.handleLog
    # Log.interpret (\(Log level msg next) -> Run.liftEffect (Ref.modify_ (_ <> [ Tuple level (Dodo.print Dodo.plainText Dodo.twoSpaces msg) ]) env.logs) *> pure next)
    -- Base effects
    # Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
    # Run.runBaseAff'

-- | For testing simple Run functions that don't need the whole environment.
runBaseEffects :: forall a. Run (LOG + EXCEPT String + AFF + EFFECT + ()) a -> Aff a
runBaseEffects = do
  Log.interpret (\(Log _ _ next) -> pure next)
    -- Base effects
    >>> Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
    >>> Run.runBaseAff'

runLegacyCacheMemory :: forall r a. CacheRef -> Run (LEGACY_CACHE + LOG + EFFECT + r) a -> Run (LOG + EFFECT + r) a
runLegacyCacheMemory = Cache.interpret Legacy.Manifest._legacyCache <<< Cache.handleMemory

runGitHubCacheMemory :: forall r a. CacheRef -> Run (GITHUB_CACHE + LOG + EFFECT + r) a -> Run (LOG + EFFECT + r) a
runGitHubCacheMemory = Cache.interpret GitHub._githubCache <<< Cache.handleMemory

runCompilerCacheMock :: forall r a. Run (COMPILER_CACHE + LOG + r) a -> Run (LOG + r) a
runCompilerCacheMock = Cache.interpret API._compilerCache case _ of
  Cache.Get key -> Exists.runExists getImpl (Cache.encodeFs key)
  Cache.Put _ next -> pure next
  Cache.Delete key -> Exists.runExists deleteImpl (Cache.encodeFs key)
  where
  getImpl :: forall x z. Cache.FsEncoding Cache.Reply x z -> Run _ x
  getImpl = case _ of
    Cache.AsBuffer _ (Cache.Reply reply) -> pure $ reply Nothing
    Cache.AsJson _ _ (Cache.Reply reply) -> pure $ reply Nothing

  deleteImpl :: forall x z. Cache.FsEncoding Cache.Ignore x z -> Run _ x
  deleteImpl = case _ of
    Cache.AsBuffer _ (Cache.Ignore next) -> pure next
    Cache.AsJson _ _ (Cache.Ignore next) -> pure next

type PursuitMockEnv =
  { excludes :: Set PackageName
  , metadataRef :: Ref (Map PackageName Metadata)
  }

-- | A mock implementation for Pursuit, which assumes a shared metadata ref with
-- | the REGISTRY effect handler. All packages present in the metadata ref are
-- | considered published, so 'Publish' is a no-op and 'GetPublishedVersions'
-- | reads the metadata ref.
-- |
-- | The is 'excludes' option allows us to manually choose packages that should
-- | NOT have their docs "published", so that we can test things like retrying
-- | the publish pipeline for Pursuit publishing only.
handlePursuitMock :: forall r a. PursuitMockEnv -> Pursuit a -> Run (EFFECT + r) a
handlePursuitMock { excludes, metadataRef } = case _ of
  Publish _json reply ->
    pure $ reply $ Right unit
  GetPublishedVersions name reply | Set.member name excludes ->
    pure $ reply $ Right Map.empty
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
    case ManifestIndex.insert ManifestIndex.ConsiderRanges manifest index of
      Left err -> pure $ reply $ Left $ "Failed to insert manifest:\n" <> Utils.unsafeStringify manifest <> " due to an error:\n" <> Utils.unsafeStringify err
      Right index' -> do
        Run.liftEffect (Ref.write index' env.indexRef)
        pure $ reply $ Right unit

  DeleteManifest name version reply -> do
    index <- Run.liftEffect (Ref.read env.indexRef)
    case ManifestIndex.delete ManifestIndex.ConsiderRanges name version index of
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
    pure $ reply $ Right $ Left ""
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
    Run.liftAff (Aff.attempt (FS.Aff.stat sourcePath)) >>= case _ of
      Left _ -> pure $ reply $ Left $ "Cannot copy " <> sourcePath <> " because it does not exist in download directory."
      Right _ -> do
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

type SourceMockEnv = { github :: FilePath }

handleSourceMock :: forall r a. SourceMockEnv -> Source a -> Run (EXCEPT String + AFF + EFFECT + r) a
handleSourceMock env = case _ of
  Fetch destination location ref reply -> do
    now <- Run.liftEffect Now.nowDateTime
    case location of
      Git _ -> pure $ reply $ Left "Packages cannot be published from Git yet (only GitHub)."
      GitHub { subdir } | isJust subdir -> pure $ reply $ Left "Packages cannot use the 'subdir' key yet."
      GitHub { repo } -> do
        let
          name = stripPureScriptPrefix repo
          fixedRef = fromMaybe ref $ String.stripPrefix (String.Pattern "v") ref
          dirname = name <> "-" <> fixedRef
          localPath = Path.concat [ env.github, dirname ]
          destinationPath = Path.concat [ destination, dirname <> "-checkout" ]
        Run.liftAff (Aff.attempt (FS.Aff.stat localPath)) >>= case _ of
          Left _ -> pure $ reply $ Left $ "Cannot copy " <> localPath <> " because it does not exist."
          Right _ -> do
            Run.liftAff $ FS.Extra.copy { from: localPath, to: destinationPath, preserveTimestamps: true }
            case pursPublishMethod of
              LegacyPursPublish -> do
                -- When using the compiler and legacy 'purs publish' we have to be
                -- in a clean git repository with the ref checked out.
                Run.liftAff $ FS.Aff.rm' (Path.concat [ destinationPath, ".git" ]) { recursive: true, force: true, maxRetries: 10, retryDelay: 1000 }
                Run.liftAff $ FS.Aff.writeTextFile UTF8 (Path.concat [ destinationPath, ".gitignore" ]) "output"
                let exec args = void (Git.withGit destinationPath args identity)
                exec [ "init" ]
                exec [ "config", "user.name", "test-user" ]
                exec [ "config", "user.email", "test-user@aol.com" ]
                exec [ "config", "commit.gpgSign", "false" ]
                exec [ "config", "tag.gpgSign", "false" ]
                exec [ "add", "." ]
                exec [ "commit", "-m", "Initial commit" ]
                exec [ "tag", "-m", ref, ref ]

              PursPublish ->
                Except.throw "Tests are not set up for 'PursPublish' and must be fixed."

            pure $ reply $ Right { path: destinationPath, published: now }

type GitHubMockEnv = { github :: FilePath }

-- | We mock GitHub by placing some repositories in the fixtures on the file
-- | system, so you can interact with the file system as if it's a remote set
-- | of repositories.
handleGitHubMock :: forall r a. GitHubMockEnv -> GitHub a -> Run (AFF + r) a
handleGitHubMock env = case _ of
  ListTags address reply -> do
    paths <- Run.liftAff $ FS.Aff.readdir env.github

    let
      name = stripPureScriptPrefix address.repo
      extractVersion = String.stripPrefix (String.Pattern (name <> "-"))
      buildTag version = do
        let sha = "c5b97d5ae6c19d5c5df71a34c7fbeeda2479ccbc"
        { name: "v" <> version
        , sha
        , url: "https://api.github.com/repos/" <> address.owner <> "/" <> address.repo <> "/commits/" <> sha
        }
      tags = Array.mapMaybe (map buildTag <<< extractVersion) paths

    pure $ reply $ Right tags

  ListTeamMembers team reply -> pure $ reply $ case team of
    { org: "purescript", team: "packaging" } -> Right [ "pacchettibotti", "f-f", "thomashoneyman" ]
    _ -> Left $ APIError { statusCode: 404, message: "No fixture provided for team " <> team.org <> "/" <> team.team }

  GetContent address ref path reply -> do
    let
      name = stripPureScriptPrefix address.repo
      fixedRef = fromMaybe ref $ String.stripPrefix (String.Pattern "v") ref
      localPath = Path.concat [ env.github, name <> "-" <> fixedRef, path ]

    result <- Run.liftAff $ Aff.attempt (FS.Aff.readTextFile UTF8 localPath) >>= case _ of
      Left _ -> pure $ Left $ APIError { statusCode: 404, message: "Not Found" }
      Right contents -> pure $ Right contents

    pure $ reply result

  -- FIXME: Respond with an actual commit for specific input paths? This isn't
  -- currently used in tests.
  GetRefCommit _address _ref reply ->
    pure $ reply $ Left $ UnexpectedError "Unimplemented"

  -- FIXME: Respond with an actual datetime for specific inputs? This isn't
  -- currently used in tests.
  GetCommitDate _address _ref reply ->
    pure $ reply $ Left $ UnexpectedError "Unimplemented"
