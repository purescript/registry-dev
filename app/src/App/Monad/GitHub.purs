-- | An effect for making requests to GitHub for various resources.
module Registry.App.Monad.GitHub where

import Registry.App.Prelude

import Control.Monad.Reader (class MonadAsk, asks)
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
import Registry.App.Legacy.Types (RawVersion(..))
import Registry.App.Monad.Cache (class FsEncodable, class MemoryEncodable, class MonadCache, FsEncoding(..), MemoryEncoding(..))
import Registry.App.Monad.Cache as Cache
import Registry.App.Monad.Log (class MonadLog)
import Registry.App.Monad.Log as Log
import Registry.Foreign.JsonRepair as JsonRepair
import Registry.Foreign.Octokit (Address, GitHubError(..), GitHubRoute(..), Octokit, Request, Tag, Team)
import Registry.Foreign.Octokit as Octokit
import Registry.Internal.Codec as Internal.Codec

-- | A key type for the GitHub cache, which stores responses from the GitHub
-- | API as JSON.
data GitHubCache (c :: Type -> Type -> Type) a = Request GitHubRoute (c (Either GitHubError RequestResult) a)

instance MemoryEncodable GitHubCache where
  encodeMemory = case _ of
    Request route k -> Exists.mkExists $ Key ("Request__" <> Octokit.printGitHubRoute route) k

instance FsEncodable GitHubCache where
  encodeFs = case _ of
    Request route k -> do
      let codec = CA.Common.either Octokit.githubErrorCodec requestResultCodec
      Exists.mkExists $ AsJson ("Request__" <> Octokit.printGitHubRoute route) codec k

-- | A class for interacting with routes in the GitHub API
class Monad m <= MonadGitHub m where
  listTags :: Address -> m (Either GitHubError (Array Tag))
  listTeamMembers :: Team -> m (Either GitHubError (Array String))
  getContent :: Address -> RawVersion -> FilePath -> m (Either GitHubError String)
  getRefCommit :: Address -> RawVersion -> m (Either GitHubError String)
  getCommitDate :: Address -> String -> m (Either GitHubError DateTime)

instance MonadGitHub m => MonadGitHub (ExceptT e m) where
  listTags = lift <<< listTags
  listTeamMembers = lift <<< listTeamMembers
  getContent address ref = lift <<< getContent address ref
  getRefCommit address = lift <<< getRefCommit address
  getCommitDate address = lift <<< getCommitDate address

-- | Read the content of a JSON file in the provided repo, decoding its contents.
getJsonFile :: forall m a. MonadGitHub m => Address -> RawVersion -> JsonCodec a -> FilePath -> m (Either GitHubError a)
getJsonFile address ref codec path = do
  content <- getContent address ref path
  let
    attemptDecode inner = case Argonaut.Parser.jsonParser (JsonRepair.tryRepair inner) of
      Left jsonError -> Left $ Octokit.DecodeError $ "Not Json: " <> jsonError
      Right json -> case CA.decode codec json of
        Left decodeError -> Left $ Octokit.DecodeError $ CA.printJsonDecodeError decodeError
        Right decoded -> Right decoded
  pure $ attemptDecode =<< content

type OctokitEnv =
  { octokit :: Octokit
  }

type GITHUB r = (github :: OctokitEnv | r)

handleListTags
  :: forall m r
   . MonadAsk { | GITHUB + r } m
  => MonadCache GitHubCache m
  => MonadLog m
  => MonadAff m
  => Address
  -> m (Either GitHubError (Array Tag))
handleListTags address = do
  env <- asks _.github
  Log.debug $ "Listing tags for " <> address.owner <> "/" <> address.repo
  request env.octokit (Octokit.listTagsRequest address)

handleListTeamMembers
  :: forall m r
   . MonadAsk { | GITHUB + r } m
  => MonadCache GitHubCache m
  => MonadLog m
  => MonadAff m
  => Team
  -> m (Either GitHubError (Array String))
handleListTeamMembers team = do
  env <- asks _.github
  Log.debug $ "Listing members of team " <> team.org <> "/" <> team.team
  result <- request env.octokit (Octokit.listTeamMembersRequest team)
  pure $ map (map _.login) result

handleGetContent
  :: forall m r
   . MonadAsk { | GITHUB + r } m
  => MonadCache GitHubCache m
  => MonadLog m
  => MonadAff m
  => Address
  -> RawVersion
  -> FilePath
  -> m (Either GitHubError String)
handleGetContent address (RawVersion ref) path = do
  env <- asks _.github
  Log.debug $ "Fetching content from " <> address.owner <> "/" <> address.repo <> " at ref " <> ref <> " at path " <> path
  request env.octokit (Octokit.getContentRequest { address, ref, path }) >>= case _ of
    Left error -> pure $ Left error
    Right result -> case Octokit.decodeBase64Content result of
      Left base64Error -> do
        Log.debug $ "Failed to decode base64-encoded file: " <> base64Error
        pure $ Left $ DecodeError base64Error
      Right decoded ->
        pure $ Right decoded

handleGetRefCommit
  :: forall m r
   . MonadAsk { | GITHUB + r } m
  => MonadCache GitHubCache m
  => MonadLog m
  => MonadAff m
  => Address
  -> RawVersion
  -> m (Either GitHubError String)
handleGetRefCommit address (RawVersion ref) = do
  env <- asks _.github
  Log.debug $ "Fetching commit associated with ref " <> ref <> " on repository " <> address.owner <> "/" <> address.repo
  request env.octokit (Octokit.getRefCommitRequest { address, ref })

handleGetCommitDate
  :: forall m r
   . MonadAsk { | GITHUB + r } m
  => MonadCache GitHubCache m
  => MonadLog m
  => MonadAff m
  => Address
  -> String
  -> m (Either GitHubError DateTime)
handleGetCommitDate address commitSha = do
  env <- asks _.github
  Log.debug $ "Fetching commit date associated with commit sha " <> commitSha <> " on repository " <> address.owner <> "/" <> address.repo
  request env.octokit (Octokit.getCommitDateRequest { address, commitSha })

-- | A helper function for implementing GET requests to the GitHub API that
-- | relies on the GitHub API to report whether there is any new data, and falls
-- | back to the cache if there is not.
request :: forall m a. MonadCache GitHubCache m => MonadLog m => MonadAff m => Octokit -> Request a -> m (Either GitHubError a)
request octokit githubRequest@{ route: route@(GitHubRoute method _ _), codec } = do
  let printedRoute = Octokit.printGitHubRoute route
  -- We cache GET requests, other than requests to fetch the current rate limit.
  case method of
    GET | route /= Octokit.rateLimitRequest.route -> do
      now <- nowUTC
      entry <- Cache.get (Request route)
      case entry of
        Nothing -> do
          result <- requestWithBackoff octokit githubRequest
          Cache.put (Request route) (result <#> \response -> { modified: now, etag: Nothing, response: CA.encode codec response })
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
            Cache.delete (Request route)
            request octokit githubRequest

          Right prevResponse -> case CA.decode codec prevResponse.response of
            Left err -> do
              Log.debug $ "Could not decode previous response data using the provided codec: " <> CA.printJsonDecodeError err
              Log.debug $ "This indicates an out-of-date cache entry. Clearing cache for route " <> printedRoute
              Cache.delete (Request route)
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
                  Cache.put (Request route) (Left otherError)
                  pure (Left otherError)
                Right valid -> do
                  Cache.put (Request route) (Right { response: CA.encode codec valid, modified: now, etag: Nothing })
                  pure $ Right valid

            -- Since we don't have support for conditional requests via etags, we'll instead
            -- auto-expire cache entries. We will be behind GitHub at most this amount per repo.
            --
            -- TODO: This 'diff' check should be removed once we have conditional requests.
            Right _ | DateTime.diff now prevResponse.modified >= Duration.Hours 4.0 -> do
              Log.debug $ "Found cache entry but it was modified more than 4 hours ago, refetching " <> printedRoute
              result <- requestWithBackoff octokit githubRequest
              Cache.put (Request route) (result <#> \resp -> { response: CA.encode codec resp, modified: now, etag: Nothing })
              pure result

            Right decoded -> do
              Log.debug $ "Found valid cache entry without etags for " <> printedRoute <> ", and within 1 hour time limit, returning value..."
              pure $ Right decoded

    _ -> do
      Log.debug $ "Not a cacheable route: " <> printedRoute <> ", requesting without the cache..."
      requestWithBackoff octokit githubRequest

-- | Apply exponential backoff to requests that hang, but without cancelling
-- | requests if we have reached our rate limit and have been throttled.
requestWithBackoff :: forall m a. MonadLog m => MonadAff m => Octokit -> Request a -> m (Either Octokit.GitHubError a)
requestWithBackoff octokit githubRequest = do
  Log.debug $ "Making request to " <> Octokit.printGitHubRoute githubRequest.route
  let action = Octokit.request octokit githubRequest
  result <- liftAff $ withBackoff
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
