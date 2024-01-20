-- | An effect for making requests to GitHub for various resources.
module Registry.App.Effect.GitHub
  ( GITHUB
  , GITHUB_CACHE
  , GitHub(..)
  , GitHubCache
  , _github
  , _githubCache
  , getCommitDate
  , getContent
  , getJsonFile
  , getRefCommit
  , handle
  , interpret
  , listTags
  , listTeamMembers
  ) where

import Registry.App.Prelude

import Data.Argonaut.Parser as Argonaut.Parser
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Exists as Exists
import Data.HTTP.Method (Method(..))
import Data.Time.Duration as Duration
import Foreign.Object as Object
import Registry.App.Effect.Cache (class FsEncodable, class MemoryEncodable, Cache, CacheRef, FsEncoding(..), MemoryEncoding(..))
import Registry.App.Effect.Cache as Cache
import Registry.App.Effect.Env (RESOURCE_ENV)
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Legacy.Types (RawVersion(..))
import Registry.Foreign.JsonRepair as JsonRepair
import Registry.Foreign.Octokit (Address, GitHubError(..), GitHubRoute(..), Octokit, Request, Tag, Team)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Codec as Internal.Codec
import Run (AFF, EFFECT, Run)
import Run as Run
import Type.Proxy (Proxy(..))

-- | A cache for the GITHUB effect, which stores the JSON content of requests.
type GITHUB_CACHE r = (githubCache :: Cache GitHubCache | r)

_githubCache :: Proxy "githubCache"
_githubCache = Proxy

-- | A key type for the GitHub cache, which stores responses from the GitHub
-- | API as JSON.
--
-- NOTE: We could also store typed responses to individual requests, but since
-- the request data types already carry around their codec it's easiest to just
-- have the single key.
data GitHubCache (c :: Type -> Type -> Type) a = Request GitHubRoute (c (Either GitHubError RequestResult) a)

instance Functor2 c => Functor (GitHubCache c) where
  map k (Request route a) = Request route (map2 k a)

instance MemoryEncodable GitHubCache where
  encodeMemory = case _ of
    Request route next -> Exists.mkExists $ Key (Octokit.printGitHubRoute route) next

instance FsEncodable GitHubCache where
  encodeFs = case _ of
    Request route next -> do
      let codec = CA.Common.either Octokit.githubErrorCodec requestResultCodec
      Exists.mkExists $ AsJson ("Request__" <> Octokit.printGitHubRoute route) codec next

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

-- | List all tags published to the provided repo.
listTags :: forall r. Address -> Run (GITHUB + r) (Either GitHubError (Array Tag))
listTags address = Run.lift _github (ListTags address identity)

-- | List the members of the provided team. Requires that the authorization on
-- | the request has read rights for the given organization and team.
listTeamMembers :: forall r. Team -> Run (GITHUB + r) (Either GitHubError (Array String))
listTeamMembers team = Run.lift _github (ListTeamMembers team identity)

-- | Read the content of a file in the provided repo.
getContent :: forall r. Address -> RawVersion -> FilePath -> Run (GITHUB + r) (Either GitHubError String)
getContent address (RawVersion ref) path = Run.lift _github (GetContent address ref path identity)

-- | Read the content of a JSON file in the provided repo, decoding its contents.
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

-- | Get the commit SHA associated with a ref on the provided repo.
getRefCommit :: forall r. Address -> RawVersion -> Run (GITHUB + r) (Either GitHubError String)
getRefCommit address (RawVersion ref) = Run.lift _github (GetRefCommit address ref identity)

-- | Get the published date of a commit SHA
getCommitDate :: forall r. Address -> String -> Run (GITHUB + r) (Either GitHubError DateTime)
getCommitDate address commitSha = Run.lift _github (GetCommitDate address commitSha identity)

-- | Interpret the GITHUB effect, given a handler.
interpret :: forall r a. (GitHub ~> Run r) -> Run (GITHUB + r) a -> Run r a
interpret handler = Run.interpret (Run.on _github handler Run.send)

type GitHubEnv =
  { ref :: CacheRef
  , cache :: FilePath
  , octokit :: Octokit
  }

-- | An effectful handler for the GITHUB effect which makes calls to GitHub
-- | using Octokit. This handler consumes a GITHUB_CACHE by caching in memory
-- | with the filesystem as a fallback.
handle :: forall r a. GitHubEnv -> GitHub a -> Run (RESOURCE_ENV + LOG + AFF + EFFECT + r) a
handle env = Cache.interpret _githubCache (Cache.handleMemoryFs { cache: env.cache, ref: env.ref }) <<< case _ of
  ListTags address reply -> do
    Log.debug $ "Listing tags for " <> address.owner <> "/" <> address.repo
    result <- request env.octokit (Octokit.listTagsRequest address)
    pure $ reply result

  ListTeamMembers team reply -> do
    Log.debug $ "Listing members of team " <> team.org <> "/" <> team.team
    result <- request env.octokit (Octokit.listTeamMembersRequest team)
    pure $ reply $ map (map _.login) result

  GetContent address ref path reply -> do
    Log.debug $ "Fetching content from " <> address.owner <> "/" <> address.repo <> " at ref " <> ref <> " at path " <> path
    request env.octokit (Octokit.getContentRequest { address, ref, path }) >>= case _ of
      Left error -> pure $ reply $ Left error
      Right result -> case Octokit.decodeBase64Content result of
        Left base64Error -> do
          Log.debug $ "Failed to decode base64-encoded file: " <> base64Error
          pure $ reply $ Left $ DecodeError base64Error
        Right decoded ->
          pure $ reply $ Right decoded

  GetRefCommit address ref reply -> do
    Log.debug $ "Fetching commit associated with ref " <> ref <> " on repository " <> address.owner <> "/" <> address.repo
    result <- request env.octokit (Octokit.getRefCommitRequest { address, ref })
    pure $ reply result

  GetCommitDate address commitSha reply -> do
    Log.debug $ "Fetching commit date associated with commit sha " <> commitSha <> " on repository " <> address.owner <> "/" <> address.repo
    result <- request env.octokit (Octokit.getCommitDateRequest { address, commitSha })
    pure $ reply result

-- | A helper function for implementing GET requests to the GitHub API that
-- | relies on the GitHub API to report whether there is any new data, and falls
-- | back to the cache if there is not.
request :: forall r a. Octokit -> Request a -> Run (RESOURCE_ENV + GITHUB_CACHE + LOG + AFF + EFFECT + r) (Either GitHubError a)
request octokit githubRequest@{ route: route@(GitHubRoute method _ _), codec } = do
  let printedRoute = Octokit.printGitHubRoute route
  -- We cache GET requests, other than requests to fetch the current rate limit.
  case method of
    GET | route /= Octokit.rateLimitRequest.route -> do
      now <- nowUTC
      entry <- Cache.get _githubCache (Request route)
      case entry of
        Nothing -> do
          result <- requestWithBackoff octokit githubRequest
          Cache.put _githubCache (Request route) (result <#> \response -> { modified: now, etag: Nothing, response: CA.encode codec response })
          pure result

        Just cached -> case cached of
          Left (APIError err)
            -- We don't retry 404 errors because they indicate a missing resource.
            | err.statusCode == 404 -> do
                Log.debug "Cached entry is a 404 error, not retrying..."
                pure $ Left $ APIError err

          -- Otherwise, if we have an error in cache, we retry the request; we
          -- don't have anything usable we could return.
          Left otherError -> do
            Log.warn $ "Retrying route " <> printedRoute <> " because cache contains non-404 error:\n" <> Octokit.printGitHubError otherError
            Cache.delete _githubCache (Request route)
            request octokit githubRequest

          Right prevResponse -> case CA.decode codec prevResponse.response of
            Left err -> do
              Log.debug $ "Could not decode previous response data using the provided codec: " <> CA.printJsonDecodeError err
              Log.debug $ "This indicates an out-of-date cache entry. Clearing cache for route " <> printedRoute
              Cache.delete _githubCache (Request route)
              Log.debug "Retrying request..."
              request octokit githubRequest

            -- If we do have a usable cache value, then we will defer to GitHub's
            -- judgment on whether to use it or not. We do that by making a request
            -- with the 'If-None-Match' header. A 304 response means the resource
            -- has not changed, and GitHub promises not to consume a request if so.
            --
            -- TODO: This is not yet implemented because we don't retrieve etags
            -- from the API. This branch will never be hit. We really ought to
            -- do this, but Octokit makes it difficult to retrieve etags for
            -- the 'request' function, and 'paginate' isn't much better. Someone
            -- should do it, though!
            Right decoded | Just etag <- prevResponse.etag -> do
              Log.debug $ "Found valid cache entry with etags for " <> printedRoute
              let headers = Object.insert "If-None-Match" etag githubRequest.headers
              Log.debug $ "Verifying cached status with headers: " <> stringifyJson (CA.Common.foreignObject CA.string) headers
              let modifiedRequest = githubRequest { headers = headers }
              conditionalResponse <- requestWithBackoff octokit modifiedRequest
              case conditionalResponse of
                Left (APIError err) | err.statusCode == 304 -> do
                  Log.debug $ "Received confirmation of cache validity response from GitHub, returning cached value..."
                  pure $ Right decoded
                Left otherError -> do
                  case otherError of
                    UnexpectedError err -> do
                      Log.error $ "Failed to request " <> printedRoute <> " due to an unexpected error. Writing it to cache: " <> err
                    APIError err -> do
                      Log.error $ "Failed to request " <> printedRoute <> " due to an API error with status " <> show err.statusCode <> ". Writing it to cache: " <> err.message
                    DecodeError err -> do
                      Log.error $ "Failed to decode response to request " <> printedRoute <> ". Writing decoding error to cache: " <> err
                  Cache.put _githubCache (Request route) (Left otherError)
                  pure (Left otherError)
                Right valid -> do
                  Cache.put _githubCache (Request route) (Right { response: CA.encode codec valid, modified: now, etag: Nothing })
                  pure $ Right valid

            -- Since we don't have support for conditional requests via etags, we'll instead
            -- auto-expire cache entries. We will be behind GitHub at most this amount per repo.
            --
            -- TODO: This 'diff' check should be removed once we have conditional requests.
            Right _ | DateTime.diff now prevResponse.modified >= Duration.Hours 23.0 -> do
              Log.debug $ "Found cache entry but it was modified more than 23 hours ago, refetching " <> printedRoute
              result <- requestWithBackoff octokit githubRequest
              Cache.put _githubCache (Request route) (result <#> \resp -> { response: CA.encode codec resp, modified: now, etag: Nothing })
              pure result

            Right decoded -> do
              Log.debug $ "Found valid cache entry without etags for " <> printedRoute <> ", and within 4 hour time limit, returning value..."
              pure $ Right decoded

    _ -> do
      Log.debug $ "Not a cacheable route: " <> printedRoute <> ", requesting without the cache..."
      requestWithBackoff octokit githubRequest

-- | Apply exponential backoff to requests that hang, but without cancelling
-- | requests if we have reached our rate limit and have been throttled.
requestWithBackoff :: forall a r. Octokit -> Request a -> Run (RESOURCE_ENV + LOG + AFF + r) (Either Octokit.GitHubError a)
requestWithBackoff octokit githubRequest = do
  let route = Octokit.printGitHubRoute githubRequest.route
  { githubApiUrl } <- Env.askResourceEnv
  Log.debug $ "Making request to " <> route <> " with base URL " <> githubApiUrl
  result <- Run.liftAff do
    let
      retryOptions =
        { timeout: defaultRetry.timeout
        , retryOnCancel: defaultRetry.retryOnCancel
        , retryOnFailure: \attempt err -> case err of
            UnexpectedError _ -> false
            DecodeError _ -> false
            -- https://docs.github.com/en/rest/overview/resources-in-the-rest-api?apiVersion=2022-11-28#exceeding-the-rate-limit
            APIError { statusCode } | statusCode >= 400 && statusCode <= 500 -> false
            APIError _ -> attempt <= 3
        }
    withRetry retryOptions (Octokit.request octokit githubRequest)
  case result of
    Cancelled -> pure $ Left $ APIError { statusCode: 400, message: "Unable to reach GitHub servers." }
    Failed err -> do
      Log.debug $ "Request failed with error: " <> Octokit.printGitHubError err
      pure $ Left err
    Succeeded success -> pure $ Right success

type RequestResult =
  { modified :: DateTime
  , etag :: Maybe String
  , response :: Json
  }

requestResultCodec :: JsonCodec RequestResult
requestResultCodec = CA.Record.object "RequestResult"
  { etag: CA.Common.maybe CA.string
  , modified: Internal.Codec.iso8601DateTime
  , response: CA.json
  }
