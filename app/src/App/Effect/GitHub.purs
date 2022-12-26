-- | An effect for making requests to GitHub for various resources.
module Registry.App.Effect.GitHub
  ( GITHUB
  , GITHUB_CACHE
  , GitHubCache
  , GitHub(..)
  , _github
  , _githubCache
  , getCommitDate
  , getContent
  , getJsonFile
  , getRefCommit
  , handleGitHubOctokit
  , listTags
  , listTeamMembers
  , runGitHub
  , githubFsEncoder
  , githubMemoryEncoder
  , runGitHubCacheMemoryFs
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
import Registry.App.Effect.Log (LOG)
import Registry.App.Effect.Log as Log
import Registry.App.Effect.TypedCache (CacheKey, CacheRef, FsEncoder, FsEncoding(..), MemoryEncoder, MemoryEncoding(..), TypedCache)
import Registry.App.Effect.TypedCache as TypedCache
import Registry.App.Legacy.Types (RawVersion(..))
import Registry.Foreign.JsonRepair as JsonRepair
import Registry.Foreign.Octokit (Address, GitHubError(..), GitHubRoute(..), Octokit, Request, Tag, Team)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Codec as Internal.Codec
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
runGitHub :: forall r a. (GitHub ~> Run r) -> Run (GITHUB + r) a -> Run r a
runGitHub handler = Run.interpret (Run.on _github handler Run.send)

-- | An effectful handler for the GITHUB effect which makes calls to GitHub
-- | using Octokit.
handleGitHubOctokit :: forall r a. Octokit -> GitHub a -> Run (GITHUB_CACHE + LOG + AFF + EFFECT + r) a
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
request :: forall r a. Octokit -> Request a -> Run (GITHUB_CACHE + LOG + AFF + EFFECT + r) (Either GitHubError a)
request octokit githubRequest@{ route: route@(GitHubRoute method _ _), codec } = do
  let printedRoute = Octokit.printGitHubRoute route
  -- We cache GET requests, other than requests to fetch the current rate limit.
  case method of
    GET | route /= Octokit.rateLimitRequest.route -> do
      now <- Run.liftEffect nowUTC
      entry <- getGitHubCache (Request route)
      case entry of
        Nothing -> do
          result <- requestWithBackoff octokit githubRequest
          putGitHubCache (Request route) (result <#> \response -> { modified: now, etag: Nothing, response: CA.encode codec response })
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
            deleteGitHubCache (Request route)
            request octokit githubRequest

          Right prevResponse -> case CA.decode codec prevResponse.response of
            Left err -> do
              Log.debug $ "Could not decode previous response data using the provided codec: " <> CA.printJsonDecodeError err
              Log.debug $ "This indicates an out-of-date cache entry. Clearing cache for route " <> printedRoute
              deleteGitHubCache (Request route)
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
                  putGitHubCache (Request route) (Left otherError)
                  pure (Left otherError)
                Right valid -> do
                  putGitHubCache (Request route) (Right { response: CA.encode codec valid, modified: now, etag: Nothing })
                  pure $ Right valid

            -- Since we don't have support for conditional requests via etags, we'll instead
            -- auto-expire cache entries. We will be behind GitHub at most this amount per repo.
            --
            -- TODO: This 'diff' check should be removed once we have conditional requests.
            Right _ | DateTime.diff now prevResponse.modified >= Duration.Hours 4.0 -> do
              Log.debug $ "Found cache entry but it was modified more than 4 hours ago, refetching " <> printedRoute
              result <- requestWithBackoff octokit githubRequest
              putGitHubCache (Request route) (result <#> \resp -> { response: CA.encode codec resp, modified: now, etag: Nothing })
              pure result

            Right decoded -> do
              Log.debug $ "Found valid cache entry without etags for " <> printedRoute <> ", and within 1 hour time limit, returning value..."
              pure $ Right decoded

    _ -> do
      Log.debug $ "Not a cacheable route: " <> printedRoute <> ", requesting without the cache..."
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

-- | A key type for the GitHub cache, which stores responses from the GitHub
-- | API as JSON.
--
-- NOTE: We could also store typed responses to individual requests, but since
-- the request data types already carry around their codec it's easiest to just
-- have the single key.
data GitHubCache (c :: Type -> Type -> Type) a = Request GitHubRoute (c (Either GitHubError RequestResult) a)

instance Functor2 c => Functor (GitHubCache c) where
  map k (Request route a) = Request route (map2 k a)

-- | A cache for the GITHUB effect, which stores the JSON content of requests.
type GITHUB_CACHE r = (githubCache :: TypedCache GitHubCache | r)

_githubCache :: Proxy "githubCache"
_githubCache = Proxy

-- | Get an item from the GitHub cache according to a GitHubCache key.
getGitHubCache :: forall r a. CacheKey GitHubCache a -> Run (GITHUB_CACHE + r) (Maybe a)
getGitHubCache key = Run.lift _githubCache (TypedCache.getCache key)

-- | Write an item to the GitHub cache using a GitHubCache key.
putGitHubCache :: forall r a. CacheKey GitHubCache a -> a -> Run (GITHUB_CACHE + r) Unit
putGitHubCache key value = Run.lift _githubCache (TypedCache.putCache key value)

-- | Remove an item from the GitHub cache using a GitHubCache key.
deleteGitHubCache :: forall r a. CacheKey GitHubCache a -> Run (GITHUB_CACHE + r) Unit
deleteGitHubCache key = Run.lift _githubCache (TypedCache.deleteCache key)

githubMemoryEncoder :: MemoryEncoder GitHubCache
githubMemoryEncoder = case _ of
  Request route next -> Exists.mkExists $ Key (Octokit.printGitHubRoute route) next

githubFsEncoder :: FsEncoder GitHubCache
githubFsEncoder = case _ of
  Request route next -> do
    let codec = CA.Common.either Octokit.githubErrorCodec requestResultCodec
    Exists.mkExists $ AsJson ("Request__" <> Octokit.printGitHubRoute route) codec next

runGitHubCacheMemoryFs
  :: forall r a
   . { ref :: CacheRef, cacheDir :: FilePath }
  -> Run (GITHUB_CACHE + LOG + AFF + EFFECT + r) a
  -> Run (LOG + AFF + EFFECT + r) a
runGitHubCacheMemoryFs { ref, cacheDir } =
  TypedCache.runCacheAt _githubCache
    ( TypedCache.handleCacheMemoryFs
        { ref
        , cacheDir
        , fs: githubFsEncoder
        , memory: githubMemoryEncoder
        }
    )
