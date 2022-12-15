module Registry.App.GitHub
  ( closeIssue
  , createComment
  , getCommitDate
  , getContent
  , getRefCommit
  , listTags
  , listTeamMembers
  , module Exports
  ) where

import Registry.App.Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Formatter.DateTime as Formatter.DateTime
import Data.HTTP.Method (Method(..))
import Data.Time.Duration as Duration
import Effect.Class.Console as Console
import Foreign.Object as Object
import Registry.App.Cache (Cache)
import Registry.App.Cache as Cache
import Registry.Foreign.Octokit (Address, GitHubError(..), Tag, Team, printGitHubError) as Exports
import Registry.Foreign.Octokit as Octokit

closeIssue :: Octokit.Octokit -> Octokit.Address -> Octokit.IssueNumber -> Aff (Either Octokit.GitHubError Unit)
closeIssue octokit address issue = do
  let request = Octokit.closeIssueRequest { address, issue }
  requestWithBackoff octokit request

createComment :: Octokit.Octokit -> Octokit.Address -> Octokit.IssueNumber -> String -> Aff (Either Octokit.GitHubError Unit)
createComment octokit address issue body = do
  let request = Octokit.createCommentRequest { address, issue, body }
  requestWithBackoff octokit request

getCommitDate :: Octokit.Octokit -> Cache -> Octokit.Address -> String -> Aff (Either Octokit.GitHubError DateTime)
getCommitDate octokit cache address commitSha = do
  let request = Octokit.getCommitDateRequest { address, commitSha }
  requestWithCache octokit cache request

getContent :: Octokit.Octokit -> Cache -> Octokit.Address -> String -> FilePath -> Aff (Either Octokit.GitHubError String)
getContent octokit cache address ref path = do
  let request = Octokit.getContentRequest { address, ref, path }
  result <- requestWithCache octokit cache request
  pure (bind result (lmap Octokit.DecodeError <<< Octokit.decodeBase64Content))

getRefCommit :: Octokit.Octokit -> Cache -> Octokit.Address -> String -> Aff (Either Octokit.GitHubError String)
getRefCommit octokit cache address ref = do
  let request = Octokit.getRefCommitRequest { address, ref }
  requestWithCache octokit cache request

listTags :: Octokit.Octokit -> Cache -> Octokit.Address -> Aff (Either Octokit.GitHubError (Array Octokit.Tag))
listTags octokit cache = requestWithCache octokit cache <<< Octokit.listTagsRequest

listTeamMembers :: Octokit.Octokit -> Cache -> Octokit.Team -> Aff (Either Octokit.GitHubError (Array Octokit.TeamMember))
listTeamMembers octokit cache = requestWithCache octokit cache <<< Octokit.listTeamMembersRequest

-- | Apply exponential backoff to requests that hang, but without cancelling
-- | requests if we have reached our rate limit and have been throttled.
-- | A helper function for implementing GET requests to the GitHub API that
-- | relies on the GitHub API to report whether there is any new data, and falls
-- | back to the cache if there is not.
requestWithCache :: forall a. Octokit.Octokit -> Cache -> Octokit.Request a -> Aff (Either Octokit.GitHubError a)
requestWithCache octokit cache githubRequest@{ route: route@(Octokit.GitHubRoute method _ _), codec } = do
  -- We cache GET requests, other than requests to fetch the current rate limit.
  case method of
    GET | route /= Octokit.rateLimitRequest.route -> do
      let entryCodec = CA.Common.either Octokit.githubErrorCodec CA.Common.json
      let entryKey = Octokit.printGitHubRoute route
      entry <- liftEffect (Cache.readJsonEntry entryCodec entryKey cache)
      now <- liftEffect nowUTC
      case entry of
        Left _ -> do
          Console.debug $ "No cache entry for route " <> entryKey
          result <- requestWithBackoff octokit githubRequest
          liftEffect $ Cache.writeJsonEntry entryCodec entryKey (map (CA.encode codec) result) cache
          pure result

        Right cached -> case cached.value of
          Left (Octokit.APIError err)
            -- We don't retry 404 errors because they indicate a missing resource.
            | err.statusCode == 404 -> do
                Console.debug "Cached entry is a 404 error, not retrying..."
                pure $ Left $ Octokit.APIError err
            -- Otherwise, if we have an error in cache, we retry the request; we
            -- don't have anything usable we could return.
            | otherwise -> do
                Console.debug $ "Retrying route " <> Octokit.printGitHubRoute route <> " because cache contains non-404 error: " <> show err
                liftEffect $ cache.remove entryKey
                requestWithCache octokit cache githubRequest

          Left otherError -> do
            Console.debug "Cached entry is an unknown or decode error, not retrying..."
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
            Console.debug $ "Cache entry expired for route " <> entryKey <> ", requesting..."
            -- This is how we *would* modify the request, once GitHub works.
            let _githubTime = Formatter.DateTime.format Octokit.rfc1123Format cached.modified
            let _modifiedRequest = githubRequest { headers = Object.insert "If-Modified-Since" _githubTime githubRequest.headers }
            result <- requestWithBackoff octokit githubRequest
            case result of
              Left (Octokit.APIError err) | err.statusCode == 304 -> do
                Console.debug $ "Received confirmation of cache validity response from GitHub, reading cache value..."
                pure result
              _ -> do
                liftEffect $ Cache.writeJsonEntry entryCodec entryKey (map (CA.encode codec) result) cache
                pure result

          Right value -> case CA.decode codec value of
            Left error -> do
              Console.debug $ "Unable to decode cache entry, returning error..."
              pure $ Left $ Octokit.DecodeError $ CA.printJsonDecodeError error
            Right accepted ->
              pure $ Right accepted

    _ -> do
      Console.debug $ "Not a cacheable route: " <> Octokit.printGitHubRoute route <> ", requesting..."
      requestWithBackoff octokit githubRequest

requestWithBackoff :: forall a. Octokit.Octokit -> Octokit.Request a -> Aff (Either Octokit.GitHubError a)
requestWithBackoff octokit githubRequest = do
  Console.log $ "Making request to " <> Octokit.printGitHubRoute githubRequest.route
  let action = Octokit.request octokit githubRequest
  result <- withBackoff
    { delay: Duration.Milliseconds 5_000.0
    , action
    , shouldCancel: \_ -> Octokit.request octokit Octokit.rateLimitRequest >>= case _ of
        Right { remaining } | remaining == 0 -> pure false
        _ -> pure true
    , shouldRetry: \attempt -> if attempt <= 3 then pure (Just action) else pure Nothing
    }
  case result of
    Nothing -> pure $ Left $ Octokit.APIError { statusCode: 400, message: "Unable to reach GitHub servers." }
    Just accepted -> pure accepted
