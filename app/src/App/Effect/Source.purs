-- | An effect for fetching packages from their specified location in a manifest.
module Registry.App.Effect.Source where

import Registry.App.Prelude

import Affjax.Node as Affjax.Node
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.HTTP.Method (Method(..))
import Data.JSDate as JSDate
import Effect.Aff as Aff
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
import Registry.Foreign.Octokit as Octokit
import Registry.Foreign.Tar as Foreign.Tar
import Registry.Location as Location
import Run (AFF, EFFECT, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except

-- | An effect for fetching package sources
data Source a = Fetch FilePath Location String (Either String FetchedSource -> a)

derive instance Functor Source

type SOURCE r = (source :: Source | r)

_source :: Proxy "source"
_source = Proxy

type FetchedSource = { path :: FilePath, published :: DateTime }

-- | Fetch the provided location to the provided destination path.
fetch :: forall r. FilePath -> Location -> String -> Run (SOURCE + EXCEPT String + r) FetchedSource
fetch destination location ref = Except.rethrow =<< Run.lift _source (Fetch destination location ref identity)

-- | Run the SOURCE effect given a handler.
interpret :: forall r a. (Source ~> Run r) -> Run (SOURCE + r) a -> Run r a
interpret handler = Run.interpret (Run.on _source handler Run.send)

-- | Handle the SOURCE effect by downloading package source to the file system.
handle :: forall r a. Source a -> Run (GITHUB + LOG + AFF + EFFECT + r) a
handle = case _ of
  Fetch destination location ref reply -> map (map reply) Except.runExcept do
    Log.info $ "Fetching " <> printJson Location.codec location
    case location of
      Git _ -> do
        -- TODO: Support non-GitHub packages. Remember subdir when doing so. (See #15)
        Except.throw "Packages are only allowed to come from GitHub for now. See #15"

      GitHub { owner, repo, subdir } -> do
        -- TODO: Support subdir. In the meantime, we verify subdir is not present. (See #16)
        when (isJust subdir) $ Except.throw "`subdir` is not supported for now. See #16"

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
              repoDir = Path.concat [ destination, repo ]

              clonePackageAtTag = do
                let url = Array.fold [ "https://github.com/", owner, "/", repo ]
                let args = [ "clone", url, "--branch", ref, "--single-branch", "-c", "advice.detachedHead=false", repoDir ]
                withBackoff' (Git.gitCLI args Nothing) >>= case _ of
                  Nothing -> Aff.throwError $ Aff.error $ "Timed out attempting to clone git tag: " <> url <> " " <> ref
                  Just (Left err) -> Aff.throwError $ Aff.error err
                  Just (Right _) -> pure unit

            Run.liftAff (Aff.attempt clonePackageAtTag) >>= case _ of
              Left error -> do
                Log.error $ "Failed to clone git tag: " <> Aff.message error
                Except.throw $ "Failed to clone repository " <> owner <> "/" <> repo <> " at ref " <> ref
              Right _ -> Log.debug $ "Cloned package source to " <> repoDir

            Log.debug $ "Getting published time..."

            let
              getRefTime = do
                timestamp <- Except.rethrow =<< Run.liftAff (Git.gitCLI [ "log", "-1", "--date=iso8601-strict", "--format=%cd", ref ] (Just repoDir))
                jsDate <- Run.liftEffect $ JSDate.parse timestamp
                dateTime <- case JSDate.toDateTime jsDate of
                  Nothing -> Except.throw $ "Could not parse timestamp of git ref to a datetime given timestamp " <> timestamp <> " and parsed js date " <> JSDate.toUTCString jsDate
                  Just parsed -> pure parsed
                pure dateTime

            -- Cloning will result in the `repo` name as the directory name
            publishedTime <- Except.runExcept getRefTime >>= case _ of
              Left error -> do
                Log.error $ "Failed to get published time: " <> error
                Except.throw $ "Cloned repository " <> owner <> "/" <> repo <> " at ref " <> ref <> ", but could not read the published time from the ref."
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
                  Except.throw $ "Failed to fetch commit data associated with " <> upstream <> " at ref " <> ref
                Right result -> pure result
              GitHub.getCommitDate { owner, repo } commit >>= case _ of
                Left githubError -> do
                  Log.error $ "Failed to fetch " <> upstream <> " at commit " <> commit <> ": " <> Octokit.printGitHubError githubError
                  Except.throw $ "Unable to get published time for commit " <> commit <> " associated with the given ref " <> ref
                Right a -> pure a

            let tarballName = ref <> ".tar.gz"
            let absoluteTarballPath = Path.concat [ destination, tarballName ]
            let archiveUrl = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> tarballName
            Log.debug $ "Fetching tarball from GitHub: " <> archiveUrl

            response <- Run.liftAff $ withBackoff' $ Affjax.Node.request $ Affjax.Node.defaultRequest
              { method = Left GET
              , responseFormat = ResponseFormat.arrayBuffer
              , url = archiveUrl
              }

            case response of
              Nothing -> Except.throw $ "Could not download " <> archiveUrl
              Just (Left error) -> do
                Log.error $ "Failed to download " <> archiveUrl <> " because of an HTTP error: " <> Affjax.Node.printError error
                Except.throw $ "Could not download " <> archiveUrl
              Just (Right { status, body }) | status /= StatusCode 200 -> do
                buffer <- Run.liftEffect $ Buffer.fromArrayBuffer body
                bodyString <- Run.liftEffect $ Buffer.toString UTF8 (buffer :: Buffer)
                Log.error $ "Failed to download " <> archiveUrl <> " because of a non-200 status code (" <> show status <> ") with body " <> bodyString
                Except.throw $ "Could not download " <> archiveUrl
              Just (Right { body }) -> do
                Log.debug $ "Successfully downloaded " <> archiveUrl <> " into a buffer."
                buffer <- Run.liftEffect $ Buffer.fromArrayBuffer body
                Run.liftAff (Aff.attempt (FS.Aff.writeFile absoluteTarballPath buffer)) >>= case _ of
                  Left error -> do
                    Log.error $ "Downloaded " <> archiveUrl <> " but failed to write it to the file at path " <> absoluteTarballPath <> ":\n" <> Aff.message error
                    Except.throw $ "Could not download " <> archiveUrl <> " due to an internal error."
                  Right _ ->
                    Log.debug $ "Tarball downloaded to " <> absoluteTarballPath

            Log.debug "Verifying tarball..."
            Foreign.Tar.getToplevelDir absoluteTarballPath >>= case _ of
              Nothing ->
                Except.throw "Downloaded tarball from GitHub has no top-level directory."
              Just path -> do
                Log.debug "Extracting the tarball..."
                Tar.extract { cwd: destination, archive: tarballName }
                pure { path, published: commitDate }
