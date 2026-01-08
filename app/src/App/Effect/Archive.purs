-- | An effect for fetching packages from the registry-archive.
-- |
-- | The registry-archive stores tarballs for packages whose original GitHub
-- | repositories are no longer available. This effect provides operations to
-- | fetch source code and metadata from that archive.
-- |
-- | This effect can be removed when the legacy importer is no longer in use.
module Registry.App.Effect.Archive
  ( ARCHIVE
  , Archive(..)
  , ArchiveError(..)
  , FetchedSource
  , _archive
  , fetch
  , fetchEither
  , handle
  , handleMock
  , interpret
  , printArchiveError
  , registryArchiveUrl
  ) where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Map as Map
import Effect.Aff as Aff
import Effect.Exception as Exception
import Fetch.Retry as Fetch
import JSON as JSON
import JSON.Object as JSON.Object
import Node.Buffer as Buffer
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.CLI.Tar as Tar
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.Types (RawVersion(..))
import Registry.Constants as Constants
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tar as Foreign.Tar
import Registry.Internal.Format as Internal.Format
import Registry.Metadata (Metadata(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Version)
import Registry.Version as Version
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

-- | The base URL for fetching tarballs from the registry archive.
registryArchiveUrl :: String
registryArchiveUrl = "https://raw.githubusercontent.com/purescript/registry-archive/main"

-- | The result of fetching source code from the registry archive.
type FetchedSource =
  { path :: FilePath
  , published :: DateTime
  }

-- | Errors that can occur when fetching from the archive.
data ArchiveError
  = DownloadFailed PackageName Version String
  | ExtractionFailed PackageName Version String
  | PublishedTimeNotFound PackageName Version

printArchiveError :: ArchiveError -> String
printArchiveError = case _ of
  DownloadFailed name version reason -> Array.fold
    [ "Failed to download "
    , formatPackageVersion name version
    , " from the registry archive: "
    , reason
    ]
  ExtractionFailed name version reason -> Array.fold
    [ "Failed to extract "
    , formatPackageVersion name version
    , " from the registry archive: "
    , reason
    ]
  PublishedTimeNotFound name version -> Array.fold
    [ "Could not find published time for "
    , formatPackageVersion name version
    ]

-- | The Archive effect, which describes fetching package tarballs from the
-- | registry-archive repository.
data Archive a = Fetch FilePath PackageName Version (Either ArchiveError FetchedSource -> a)

derive instance Functor Archive

type ARCHIVE r = (archive :: Archive | r)

_archive :: Proxy "archive"
_archive = Proxy

-- | Fetch a package tarball from the registry archive, extracting it to the
-- | given destination directory. Returns the path to the extracted source and
-- | the published time.
fetch :: forall r. FilePath -> PackageName -> Version -> Run (ARCHIVE + EXCEPT String + r) FetchedSource
fetch destination name version = (Except.rethrow <<< lmap printArchiveError) =<< fetchEither destination name version

-- | Fetch a package tarball from the registry archive, returning the typed
-- | ArchiveError on failure.
fetchEither :: forall r. FilePath -> PackageName -> Version -> Run (ARCHIVE + r) (Either ArchiveError FetchedSource)
fetchEither destination name version = Run.lift _archive (Fetch destination name version identity)

-- | Run the ARCHIVE effect given a handler.
interpret :: forall r a. (Archive ~> Run r) -> Run (ARCHIVE + r) a -> Run r a
interpret handler = Run.interpret (Run.on _archive handler Run.send)

-- | Handle the ARCHIVE effect by fetching from the real registry-archive on GitHub.
handle :: forall r a. Archive a -> Run (GITHUB + LOG + AFF + EFFECT + r) a
handle = case _ of
  Fetch destination name version reply -> do
    result <- fetchFromArchiveImpl destination name version
    pure $ reply result

-- | Internal implementation that fetches from the registry-archive and looks up
-- | the published time from the remote registry metadata.
fetchFromArchiveImpl
  :: forall r
   . FilePath
  -> PackageName
  -> Version
  -> Run (GITHUB + LOG + AFF + EFFECT + r) (Either ArchiveError FetchedSource)
fetchFromArchiveImpl destination name version = do
  let
    nameStr = PackageName.print name
    versionStr = Version.print version
    tarballName = versionStr <> ".tar.gz"
    -- Extract to a subdirectory to avoid path collisions with the packaging
    -- directory (which uses the name-version format that archive tarballs
    -- also use internally).
    extractDir = Path.concat [ destination, "archive" ]
    absoluteTarballPath = Path.concat [ extractDir, tarballName ]
    archiveUrl = Array.fold
      [ registryArchiveUrl
      , "/"
      , nameStr
      , "/"
      , versionStr
      , ".tar.gz"
      ]

  Log.debug $ "Fetching archive tarball from: " <> archiveUrl
  FS.Extra.ensureDirectory extractDir

  response <- Run.liftAff $ Fetch.withRetryRequest archiveUrl {}

  case response of
    Cancelled ->
      pure $ Left $ DownloadFailed name version "Request was cancelled"
    Failed (Fetch.FetchError error) -> do
      Log.error $ "HTTP error when fetching archive: " <> Exception.message error
      pure $ Left $ DownloadFailed name version (Exception.message error)
    Failed (Fetch.StatusError { status, arrayBuffer: arrayBufferAff }) -> do
      arrayBuffer <- Run.liftAff arrayBufferAff
      buffer <- Run.liftEffect $ Buffer.fromArrayBuffer arrayBuffer
      bodyString <- Run.liftEffect $ Buffer.toString UTF8 (buffer :: Buffer)
      Log.error $ Array.fold
        [ "Bad status ("
        , show status
        , ") when fetching archive with body: "
        , bodyString
        ]
      pure $ Left $ DownloadFailed name version ("HTTP status " <> show status)
    Succeeded { arrayBuffer: arrayBufferAff } -> do
      arrayBuffer <- Run.liftAff arrayBufferAff
      buffer <- Run.liftEffect $ Buffer.fromArrayBuffer arrayBuffer
      Run.liftAff (Aff.attempt (FS.Aff.writeFile absoluteTarballPath buffer)) >>= case _ of
        Left error -> do
          Log.error $ Array.fold
            [ "Downloaded archive but failed to write to "
            , absoluteTarballPath
            , ": "
            , Aff.message error
            ]
          pure $ Left $ DownloadFailed name version "Failed to write tarball to disk"
        Right _ -> do
          Log.debug $ "Tarball downloaded to " <> absoluteTarballPath
          Foreign.Tar.getToplevelDir absoluteTarballPath >>= case _ of
            Nothing ->
              pure $ Left $ ExtractionFailed name version "Tarball has no top-level directory"
            Just extractedPath -> do
              Log.debug "Extracting archive tarball..."
              Tar.extract { cwd: extractDir, archive: tarballName }
              fetchRemotePublishedTime name version >>= case _ of
                Nothing -> pure $ Left $ PublishedTimeNotFound name version
                Just publishedTime ->
                  pure $ Right { path: Path.concat [ extractDir, extractedPath ], published: publishedTime }

-- | Fetch the published time for a specific version from the remote registry
-- | repo (main branch). Used as a fallback when the local registry checkout
-- | doesn't have metadata for archive-backed packages.
fetchRemotePublishedTime :: forall r. PackageName -> Version -> Run (GITHUB + LOG + r) (Maybe DateTime)
fetchRemotePublishedTime name version = do
  let
    printed = PackageName.print name
    path = Path.concat [ Constants.metadataDirectory, printed <> ".json" ]
  Log.debug $ Array.fold
    [ "Fetching published time for "
    , formatPackageVersion name version
    , " from remote registry"
    ]
  GitHub.getContent Constants.registry (RawVersion "main") path >>= case _ of
    Left err -> do
      Log.warn $ Array.fold
        [ "Failed to fetch remote metadata for "
        , printed
        , ": "
        , Octokit.printGitHubError err
        ]
      pure Nothing
    Right content -> do
      let
        parsed = do
          json <- hush $ JSON.parse content
          obj <- JSON.toJObject json
          publishedJson <- JSON.Object.lookup "published" obj
          publishedObj <- JSON.toJObject publishedJson
          versionJson <- JSON.Object.lookup (Version.print version) publishedObj
          versionObj <- JSON.toJObject versionJson
          timeJson <- JSON.Object.lookup "publishedTime" versionObj
          timeStr <- JSON.toString timeJson
          hush $ Formatter.DateTime.unformat Internal.Format.iso8601DateTime timeStr
      case parsed of
        Nothing -> do
          Log.warn $ Array.fold
            [ "Could not extract publishedTime for "
            , formatPackageVersion name version
            , " from remote metadata"
            ]
          pure Nothing
        Just dt -> do
          Log.debug $ Array.fold
            [ "Fetched published time for "
            , formatPackageVersion name version
            , " from remote registry"
            ]
          pure $ Just dt

-- | A mock handler for testing that uses a local directory of tarballs instead
-- | of fetching from the remote registry-archive.
handleMock
  :: forall r a
   . { archiveDir :: FilePath, metadata :: Map PackageName Metadata }
  -> Archive a
  -> Run (LOG + AFF + EFFECT + r) a
handleMock env = case _ of
  Fetch destination name version reply -> map (map reply) Except.runExcept do
    let
      tarballName = Version.print version <> ".tar.gz"
      sourcePath = Path.concat [ env.archiveDir, PackageName.print name <> "-" <> Version.print version <> ".tar.gz" ]
      absoluteTarballPath = Path.concat [ destination, tarballName ]

    Run.liftAff (Aff.attempt (FS.Aff.stat sourcePath)) >>= case _ of
      Left _ ->
        Except.throw $ DownloadFailed name version "Tarball not found in mock archive"
      Right _ -> do
        Run.liftAff (Aff.attempt (FS.Aff.copyFile sourcePath absoluteTarballPath)) >>= case _ of
          Left error ->
            Except.throw $ DownloadFailed name version (Aff.message error)
          Right _ ->
            Log.debug $ "Copied mock tarball to " <> absoluteTarballPath

    Foreign.Tar.getToplevelDir absoluteTarballPath >>= case _ of
      Nothing ->
        Except.throw $ ExtractionFailed name version "Tarball has no top-level directory"
      Just extractedPath -> do
        Log.debug "Extracting mock archive tarball..."
        Tar.extract { cwd: destination, archive: tarballName }
        case Map.lookup name env.metadata of
          Nothing ->
            Except.throw $ PublishedTimeNotFound name version
          Just (Metadata m) ->
            case Map.lookup version m.published of
              Nothing ->
                Except.throw $ PublishedTimeNotFound name version
              Just publishedMeta ->
                pure { path: Path.concat [ destination, extractedPath ], published: publishedMeta.publishedTime }
