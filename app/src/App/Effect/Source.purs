-- | An effect for fetching packages from their specified location in a manifest.
module Registry.App.Effect.Source where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.JSDate as JSDate
import Data.String as String
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Exception as Exception
import Effect.Now as Now
import Fetch.Retry as Fetch
import Node.Buffer as Buffer
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.CLI.Git as Git
import Registry.App.CLI.Tar as Tar
import Registry.App.Effect.GitHub (GITHUB)
import Registry.App.Effect.GitHub as GitHub
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.Types (RawVersion(..))
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tar as Foreign.Tar
import Registry.Location as Location
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

-- | Packages can be published via the legacy importer or a user via the API. We
-- | determine some information differently in these cases, such as the time the
-- | package was published.
data ImportType = Old | Recent

derive instance Eq ImportType

-- | An effect for fetching package sources
data Source a = Fetch FilePath Location String (Either FetchError FetchedSource -> a)

derive instance Functor Source

type SOURCE r = (source :: Source | r)

_source :: Proxy "source"
_source = Proxy

type FetchedSource = { path :: FilePath, published :: DateTime }

data FetchError
  = GitHubOnly
  | NoSubdir
  | InaccessibleRepo Octokit.Address
  | NoToplevelDir
  | Fatal String

printFetchError :: FetchError -> String
printFetchError = case _ of
  GitHubOnly -> "Packages are only allowed to come from GitHub for now. See issue #15."
  NoSubdir -> "Monorepos and the `subdir` key are not supported yet. See issue #16."
  InaccessibleRepo { owner, repo } -> "Repository located at https://github.com/" <> owner <> "/" <> repo <> ".git is inaccessible or does not exist."
  NoToplevelDir -> "Downloaded tarball has no top-level directory."
  Fatal err -> "Unrecoverable error. " <> err

-- | Fetch the provided location to the provided destination path.
fetch :: forall r. FilePath -> Location -> String -> Run (SOURCE + EXCEPT String + r) FetchedSource
fetch destination location ref = (Except.rethrow <<< lmap printFetchError) =<< Run.lift _source (Fetch destination location ref identity)

-- | Run the SOURCE effect given a handler.
interpret :: forall r a. (Source ~> Run r) -> Run (SOURCE + r) a -> Run r a
interpret handler = Run.interpret (Run.on _source handler Run.send)

-- | Handle the SOURCE effect by downloading package source to the file system.
handle :: forall r a. ImportType -> Source a -> Run (GITHUB + LOG + AFF + EFFECT + r) a
handle importType = case _ of
  Fetch destination location ref reply -> map (map reply) Except.runExcept do
    Log.info $ "Fetching " <> printJson Location.codec location
    case location of
      Git _ -> do
        -- TODO: Support non-GitHub packages. Remember subdir when doing so. (See #15)
        Except.throw GitHubOnly

      GitHub { owner, repo, subdir } -> do
        -- TODO: Support subdir. In the meantime, we verify subdir is not present. (See #16)
        when (isJust subdir) $ Except.throw NoSubdir

        case pursPublishMethod of
          -- This needs to be removed so that we can support non-GitHub packages (#15)
          -- and monorepo packages (#16).
          --
          -- However, the PureScript compiler requires packages to be a Git repo
          -- with a tag checked out. Until we can replace using the compiler's
          -- 'publish' command for docs we have to use this hacky checkout.
          LegacyPursPublish -> do
            Log.debug $ "Using legacy Git clone to fetch package source at tag: " <> show { owner, repo, ref }

            let
              repoDir = Path.concat [ destination, repo <> "-" <> ref ]

              -- If a git clone is cancelled by the timeout, but had partially-cloned, then it will
              -- leave behind files that prevent a retry.
              retryOpts = defaultRetry
                { cleanupOnCancel = FS.Extra.remove repoDir
                , timeout = Milliseconds 15_000.0
                }

              cloneUrl =
                Array.fold [ "https://github.com/", owner, "/", repo ]

              -- We disable Git LFS smudging because package sources should not
              -- contain large binary files. This avoids downloading LFS objects
              -- from misconfigured packages.
              cloneArgs =
                [ "-c"
                , "filter.lfs.smudge=cat"
                , "-c"
                , "filter.lfs.process=cat"
                , "clone"
                , cloneUrl
                , "--branch"
                , ref
                , "--single-branch"
                , "-c"
                , "advice.detachedHead=false"
                , repoDir
                ]

              clonePackageAtTag =
                withRetry retryOpts (Git.gitCLI cloneArgs Nothing) >>= case _ of
                  Cancelled ->
                    Aff.throwError $ Aff.error $ "Timed out attempting to clone git tag: " <> cloneUrl <> " " <> ref
                  Failed err ->
                    Aff.throwError $ Aff.error err
                  Succeeded _ ->
                    pure unit

            Run.liftAff (Aff.attempt clonePackageAtTag) >>= case _ of
              Right _ -> Log.debug $ "Cloned package source to " <> repoDir
              Left error -> do
                Log.warn $ "Git clone command failed:\n  " <> String.joinWith " " (Array.cons "git" cloneArgs)
                Log.error $ "Failed to clone git tag: " <> Aff.message error

                -- We'll receive this message if we try to clone a repo which doesn't
                -- exist, which is interpreted as an attempt to fetch a private repo.
                let missingRepoErr = "fatal: could not read Username for 'https://github.com': terminal prompts disabled"

                if String.contains (String.Pattern missingRepoErr) (Aff.message error) then
                  Except.throw $ InaccessibleRepo { owner, repo }
                else
                  Except.throw $ Fatal $ "Failed to clone repository " <> owner <> "/" <> repo <> " at ref " <> ref

            Log.debug $ "Getting published time..."

            let
              getRefTime = case importType of
                Old -> do
                  timestamp <- (Except.rethrow <<< lmap Fatal) =<< Run.liftAff (Git.gitCLI [ "log", "-1", "--date=iso8601-strict", "--format=%cd", ref ] (Just repoDir))
                  jsDate <- Run.liftEffect $ JSDate.parse timestamp
                  dateTime <- case JSDate.toDateTime jsDate of
                    Nothing -> Except.throw $ Fatal $ "Could not parse timestamp of git ref to a datetime given timestamp " <> timestamp <> " and parsed js date " <> JSDate.toUTCString jsDate
                    Just parsed -> pure parsed
                  pure dateTime
                Recent ->
                  Run.liftEffect Now.nowDateTime

            -- Cloning will result in the `repo` name as the directory name
            publishedTime <- Except.runExcept getRefTime >>= case _ of
              Left error -> do
                Log.error $ "Failed to get published time. " <> printFetchError error
                Except.throw $ Fatal $ "Cloned repository " <> owner <> "/" <> repo <> " at ref " <> ref <> ", but could not read the published time from the ref."
              Right value -> pure value

            pure { path: repoDir, published: publishedTime }

          -- This method is not currently used (see the comment on LegacyPursPublish),
          -- but it's implemented here to demonstrate what we should do once we no
          -- longer have to check out the repository.
          PursPublish -> do
            Log.debug $ "Using GitHub API to fetch package source at tag " <> show { owner, repo, ref }
            commitDate <- do
              let upstream = owner <> "/" <> repo
              commit <- GitHub.getRefCommit { owner, repo } (RawVersion ref) >>= case _ of
                Left githubError -> do
                  Log.error $ "Failed to fetch " <> upstream <> " at ref " <> ref <> ": " <> Octokit.printGitHubError githubError
                  Except.throw $ Fatal $ "Failed to fetch commit data associated with " <> upstream <> " at ref " <> ref
                Right result -> pure result
              GitHub.getCommitDate { owner, repo } commit >>= case _ of
                Left githubError -> do
                  Log.error $ "Failed to fetch " <> upstream <> " at commit " <> commit <> ": " <> Octokit.printGitHubError githubError
                  Except.throw $ Fatal $ "Unable to get published time for commit " <> commit <> " associated with the given ref " <> ref
                Right a -> pure a

            let tarballName = ref <> ".tar.gz"
            let absoluteTarballPath = Path.concat [ destination, tarballName ]
            let archiveUrl = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> tarballName
            Log.debug $ "Fetching tarball from GitHub: " <> archiveUrl

            response :: RetryResult Fetch.RetryRequestError Fetch.Response <-
              Run.liftAff $ Fetch.withRetryRequest archiveUrl {}

            case response of
              Cancelled -> Except.throw $ Fatal $ "Could not download " <> archiveUrl
              Failed (Fetch.FetchError error) -> do
                Log.error $ "Failed to download " <> archiveUrl <> " because of an HTTP error: " <> Exception.message error
                Except.throw $ Fatal $ "Could not download " <> archiveUrl
              Failed (Fetch.StatusError { status, arrayBuffer: arrayBufferAff }) -> do
                arrayBuffer <- Run.liftAff arrayBufferAff
                buffer <- Run.liftEffect $ Buffer.fromArrayBuffer arrayBuffer
                bodyString <- Run.liftEffect $ Buffer.toString UTF8 (buffer :: Buffer)
                Log.error $ "Failed to download " <> archiveUrl <> " because of a non-200 status code (" <> show status <> ") with body " <> bodyString
                Except.throw $ Fatal $ "Could not download " <> archiveUrl
              Succeeded { arrayBuffer: arrayBufferAff } -> do
                arrayBuffer <- Run.liftAff arrayBufferAff
                Log.debug $ "Successfully downloaded " <> archiveUrl <> " into a buffer."
                buffer <- Run.liftEffect $ Buffer.fromArrayBuffer arrayBuffer
                Run.liftAff (Aff.attempt (FS.Aff.writeFile absoluteTarballPath buffer)) >>= case _ of
                  Left error -> do
                    Log.error $ "Downloaded " <> archiveUrl <> " but failed to write it to the file at path " <> absoluteTarballPath <> ":\n" <> Aff.message error
                    Except.throw $ Fatal $ "Could not download " <> archiveUrl <> " due to an internal error."
                  Right _ ->
                    Log.debug $ "Tarball downloaded to " <> absoluteTarballPath

            Log.debug "Verifying tarball..."
            Foreign.Tar.getToplevelDir absoluteTarballPath >>= case _ of
              Nothing ->
                Except.throw NoToplevelDir
              Just path -> do
                Log.debug "Extracting the tarball..."
                Tar.extract { cwd: destination, archive: tarballName }
                pure { path, published: commitDate }
