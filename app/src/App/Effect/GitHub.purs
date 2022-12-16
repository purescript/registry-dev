-- | An effect for making requests to GitHub for various resources.
module Registry.App.Effect.GitHub
  ( GITHUB
  , GitHub(..)
  , _github
  , getCommitDate
  , getContent
  , getJsonFile
  , getRefCommit
  , handleGitHubOctokit
  , listTags
  , listTeamMembers
  ) where

import Registry.App.Prelude

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut as CA
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Formatter.DateTime as Formatter.DateTime
import Data.HTTP.Method (Method(..))
import Data.Time.Duration as Duration
import Foreign.Object as Object
import Registry.App.Effect.Cache (CACHE)
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.Types (RawVersion(..))
import Registry.Foreign.JsonRepair as JsonRepair
import Registry.Foreign.Octokit (Address, GitHubError(..), GitHubRoute(..), Octokit, Request, Tag, Team)
import Registry.Foreign.Octokit as Octokit
import Run (AFF, EFFECT, Run)
import Run as Run
import Type.Proxy (Proxy(..))

data GitHub a
  = ListTags Address (Either GitHubError (Array Tag) -> a)
  | ListTeamMembers Team (Either GitHubError (Array String) -> a)
  | GetContent Address String FilePath (Either GitHubError String -> a)
  | GetRefCommit Address String (Either GitHubError String -> a)
  | GetCommitDate Address String (Either GitHubError DateTime -> a)

derive instance Functor GitHub

-- | An effect for reading resources from GitHub repositories
type GITHUB r = (github :: GitHub | r)

_github :: Proxy "github"
_github = Proxy

listTags :: forall r. Address -> Run (GITHUB + r) (Either GitHubError (Array Tag))
listTags address = Run.lift _github (ListTags address identity)

-- | List the members of the provided team. Requires that the authorization on
-- | the request has read rights for the given organization and team.
listTeamMembers :: forall r. Team -> Run (GITHUB + r) (Either GitHubError (Array String))
listTeamMembers team = Run.lift _github (ListTeamMembers team identity)

getContent :: forall r. Address -> RawVersion -> FilePath -> Run (GITHUB + r) (Either GitHubError String)
getContent address (RawVersion ref) path = Run.lift _github (GetContent address ref path identity)

getJsonFile :: forall r a. Address -> RawVersion -> JsonCodec a -> FilePath -> Run (GITHUB + r) (Either GitHubError a)
getJsonFile address ref codec path = do
  content <- getContent address ref path
  let
    attemptDecode inner = case Argonaut.Parser.jsonParser (JsonRepair.tryRepair inner) of
      Left jsonError -> Left $ Octokit.DecodeError $ "Not Json: " <> jsonError
      Right json -> case CA.decode codec json of
        Left decodeError -> Left $ Octokit.DecodeError $ CA.printJsonDecodeError decodeError
        Right decoded -> Right decoded
  pure $ attemptDecode =<< content

getRefCommit :: forall r. Address -> RawVersion -> Run (GITHUB + r) (Either GitHubError String)
getRefCommit address (RawVersion ref) = Run.lift _github (GetRefCommit address ref identity)

getCommitDate :: forall r. Address -> RawVersion -> Run (GITHUB + r) (Either GitHubError DateTime)
getCommitDate address (RawVersion ref) = Run.lift _github (GetCommitDate address ref identity)

-- | An effectful handler for the GITHUB effect which makes calls to GitHub
-- | using Octokit.
handleGitHubOctokit :: forall r a. Octokit -> GitHub a -> Run (CACHE + LOG + AFF + EFFECT + r) a
handleGitHubOctokit octokit = case _ of
  ListTags address reply -> do
    Log.debug $ "Listing tags for " <> address.owner <> "/" <> address.repo
    result <- request octokit (Octokit.listTagsRequest address)
    pure $ reply result

  ListTeamMembers team reply -> do
    Log.debug $ "Listing members of team " <> team.org <> "/" <> team.team
    result <- request octokit (Octokit.listTeamMembersRequest team)
    pure $ reply $ map (map _.login) result

  GetContent address ref path reply -> do
    Log.debug $ "Fetching content from " <> address.owner <> "/" <> address.repo <> " at ref " <> ref <> " at path " <> path
    request octokit (Octokit.getContentRequest { address, ref, path }) >>= case _ of
      Left error -> pure $ reply $ Left error
      Right result -> case Octokit.decodeBase64Content result of
        Left base64Error -> do
          Log.debug $ "Failed to decode base64-encoded file: " <> base64Error
          pure $ reply $ Left $ DecodeError base64Error
        Right decoded ->
          pure $ reply $ Right decoded

  GetRefCommit address ref reply -> do
    Log.debug $ "Fetching commit associated with ref " <> ref <> " on repository " <> address.owner <> "/" <> address.repo
    result <- request octokit (Octokit.getRefCommitRequest { address, ref })
    pure $ reply result

  GetCommitDate address commitSha reply -> do
    Log.debug $ "Fetching commit date associated with commit sha " <> commitSha <> " on repository " <> address.owner <> "/" <> address.repo
    result <- request octokit (Octokit.getCommitDateRequest { address, commitSha })
    pure $ reply result

-- | A helper function for implementing GET requests to the GitHub API that
-- | relies on the GitHub API to report whether there is any new data, and falls
-- | back to the cache if there is not.
request :: forall r a. Octokit -> Request a -> Run (CACHE + LOG + AFF + EFFECT + r) (Either GitHubError a)
request octokit githubRequest@{ route: route@(GitHubRoute method _ _), codec } = do
  -- We cache GET requests, other than requests to fetch the current rate limit.
  case method of
    GET | route /= Octokit.rateLimitRequest.route -> do
      entry <- Cache.get (Cache.Request route)
      now <- Run.liftEffect nowUTC
      case entry of
        Nothing -> do
          Log.debug $ "No cache entry for route " <> Octokit.printGitHubRoute route
          result <- requestWithBackoff octokit githubRequest
          Cache.put (Cache.Request route) (map (CA.encode codec) result)
          pure result

        Just cached -> case cached.value of
          Left (APIError err)
            -- We don't retry 404 errors because they indicate a missing resource.
            | err.statusCode == 404 -> do
                Log.debug "Cached entry is a 404 error, not retrying..."
                pure $ Left $ APIError err
            -- Otherwise, if we have an error in cache, we retry the request; we
            -- don't have anything usable we could return.
            | otherwise -> do
                Log.debug $ "Retrying route " <> Octokit.printGitHubRoute route <> " because cache contains non-404 error: " <> show err
                Cache.delete (Cache.Request route)
                request octokit githubRequest

          Left otherError -> do
            Log.debug "Cached entry is an unknown or decode error, not retrying..."
            pure (Left otherError)

          -- If we do have a usable cache value, then we will defer to GitHub's
          -- judgment on whether to use it or not. We do that by making a request
          -- with the 'If-Not-Modified' header. A 304 response means the resource
          -- has not changed, and GitHub promises not to consume a request if so.
          --
          -- Unfortunately, GitHub is not currently (2022-07-01) honoring this
          -- promise, so we (temporarily) only retry after N hours have passed. Once
          -- they start honoring the promise again we can remove the modified time
          -- guard below.
          --
          -- TODO: Remove DateTime.diff when GitHub honors requests again.
          Right _ | DateTime.diff now cached.modified >= Duration.Hours 1.0 -> do
            let printed = Octokit.printGitHubRoute route
            Log.debug $ "Cache entry expired for route " <> printed <> ", requesting..."
            -- This is how we *would* modify the request, once GitHub works.
            let _githubTime = Formatter.DateTime.format Octokit.rfc1123Format cached.modified
            let _modifiedRequest = githubRequest { headers = Object.insert "If-Modified-Since" _githubTime githubRequest.headers }
            result <- requestWithBackoff octokit githubRequest
            case result of
              Left (APIError err) | err.statusCode == 304 -> do
                Log.debug $ "Received confirmation of cache validity response from GitHub, reading cache value..."
                pure result
              Left otherError -> do
                case otherError of
                  UnexpectedError err -> do
                    Log.error $ "Failed to request " <> printed <> " due to an unexpected error. Writing it to cache: " <> err
                  APIError err -> do
                    Log.error $ "Failed to request " <> printed <> " due to an API error with status " <> show err.statusCode <> ". Writing it to cache: " <> err.message
                  DecodeError err -> do
                    Log.error $ "Failed to decode response to request " <> printed <> ". Writing decoding error to cache: " <> err
                Cache.put (Cache.Request route) (map (CA.encode codec) result)
                pure result
              Right valid -> do
                Cache.put (Cache.Request route) (Right (CA.encode codec valid))
                pure result

          Right value -> case CA.decode codec value of
            Left error -> do
              Log.debug $ "Unable to decode cache entry, returning error..."
              pure $ Left $ DecodeError $ CA.printJsonDecodeError error
            Right accepted ->
              pure $ Right accepted

    _ -> do
      Log.debug $ "Not a cacheable route: " <> Octokit.printGitHubRoute route <> ", requesting without the cache..."
      requestWithBackoff octokit githubRequest

-- | Apply exponential backoff to requests that hang, but without cancelling
-- | requests if we have reached our rate limit and have been throttled.
requestWithBackoff :: forall a r. Octokit -> Request a -> Run (LOG + AFF + r) (Either Octokit.GitHubError a)
requestWithBackoff octokit githubRequest = do
  Log.debug $ "Making request to " <> Octokit.printGitHubRoute githubRequest.route
  let action = Octokit.request octokit githubRequest
  result <- Run.liftAff $ withBackoff
    { delay: Duration.Milliseconds 5_000.0
    , action
    , shouldCancel: \_ -> Octokit.request octokit Octokit.rateLimitRequest >>= case _ of
        Right { remaining } | remaining == 0 -> pure false
        _ -> pure true
    , shouldRetry: \attempt -> if attempt <= 3 then pure (Just action) else pure Nothing
    }
  case result of
    Nothing -> pure $ Left $ APIError { statusCode: 400, message: "Unable to reach GitHub servers." }
    Just accepted -> pure accepted
