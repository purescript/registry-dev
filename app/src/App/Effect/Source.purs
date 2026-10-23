-- | An effect for fetching packages from their specified location in a manifest.
module Registry.App.Effect.Source where

import Registry.App.Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.String as String
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Now as Now
import Node.Path as Path
import Registry.App.CLI.Git as Git
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Octokit as Octokit
import Registry.Location as Location
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

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
  -- | A transient network error occurred (timeout, DNS failure, etc.). The caller should retry later.
  | TransientError { owner :: String, repo :: String, message :: String }
  | Fatal String

printFetchError :: FetchError -> String
printFetchError = case _ of
  GitHubOnly -> "Packages are only allowed to come from GitHub for now. See issue #15."
  NoSubdir -> "Monorepos and the `subdir` key are not supported yet. See issue #16."
  InaccessibleRepo { owner, repo } -> "Repository located at https://github.com/" <> owner <> "/" <> repo <> ".git is inaccessible or does not exist."
  TransientError { owner, repo, message } -> "Transient error accessing repository " <> owner <> "/" <> repo <> ": " <> message <> ". This may succeed on retry."
  Fatal err -> "Unrecoverable error. " <> err

-- | Fetch the provided location to the provided destination path.
fetch :: forall r. FilePath -> Location -> String -> Run (SOURCE + EXCEPT String + r) FetchedSource
fetch destination location ref = (Except.rethrow <<< lmap printFetchError) =<< fetchEither destination location ref

-- | Fetch the provided location, returning the typed FetchError on failure.
fetchEither :: forall r. FilePath -> Location -> String -> Run (SOURCE + r) (Either FetchError FetchedSource)
fetchEither destination location ref = Run.lift _source (Fetch destination location ref identity)

-- | Run the SOURCE effect given a handler.
interpret :: forall r a. (Source ~> Run r) -> Run (SOURCE + r) a -> Run r a
interpret handler = Run.interpret (Run.on _source handler Run.send)

-- | Handle the SOURCE effect by downloading package source to the file system.
handle :: forall r a. Source a -> Run (LOG + AFF + EFFECT + r) a
handle = case _ of
  Fetch destination location ref reply -> map (map reply) Except.runExcept do
    Log.info $ "Fetching " <> printJson Location.codec location
    case location of
      Git _ -> do
        -- TODO: Support non-GitHub packages. Remember subdir when doing so. (See #15)
        Except.throw GitHubOnly

      GitHub { owner, repo, subdir } -> do
        -- TODO: Support subdir. In the meantime, we verify subdir is not present. (See #16)
        when (isJust subdir) $ Except.throw NoSubdir

        Log.debug $ "Using Git clone to fetch package source at tag: " <> show { owner, repo, ref }

        let
          cloneUrl =
            Array.fold [ "https://github.com/", owner, "/", repo ]

        Log.debug $ "Verifying repository is accessible: " <> cloneUrl
        Run.liftAff (Git.gitRepoIsAccessibleWithRetry cloneUrl) >>= case _ of
          Git.RepoAccessible -> pure unit
          Git.RepoNotFound -> do
            Log.error $ "Repository " <> owner <> "/" <> repo <> " does not exist or is private."
            Except.throw $ InaccessibleRepo { owner, repo }
          Git.RepoTransientError err -> do
            Log.error $ "Repository " <> owner <> "/" <> repo <> " could not be reached due to a transient error: " <> err
            Except.throw $ TransientError { owner, repo, message: err }

        let
          repoDir = Path.concat [ destination, repo <> "-" <> ref ]

          -- If a git clone is cancelled by the timeout, but had partially-cloned, then it will
          -- leave behind files that prevent a retry.
          retryOpts = defaultRetry
            { cleanupOnCancel = FS.Extra.remove repoDir
            , timeout = Milliseconds 15_000.0
            }

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
              Except.throw $ Fatal $ "Failed to clone repository " <> owner <> "/" <> repo <> " at ref " <> ref <> ": " <> Aff.message error

        Log.debug $ "Getting published time..."
        publishedTime <- Run.liftEffect Now.nowDateTime
        pure { path: repoDir, published: publishedTime }
