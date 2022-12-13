module Registry.App.GitHub
  ( Address
  , IssueNumber(..)
  , Tag
  , Team
  , TeamMember
  , closeIssue
  , createComment
  , getCommitDate
  , getContent
  , getRefCommit
  , listTags
  , listTeamMembers
  , module Exports
  , request
  , requestWithBackoff
  ) where

import Registry.App.Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Formatter.DateTime as Formatter.DateTime
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Map as Map
import Data.Profunctor as Profunctor
import Data.Time.Duration as Duration
import Effect.Class.Console as Console
import Foreign.Object as Object
import Registry.App.Cache (Cache)
import Registry.App.Cache as Cache
import Registry.Foreign.GitHub (Base64Content(..), GitHubError(..), GitHubRoute(..), Octokit, Request)
import Registry.Foreign.GitHub (GitHubError(..), GitHubToken(..), Octokit, newOctokit, printGitHubError) as Exports
import Registry.Foreign.GitHub as Foreign.GitHub
import Registry.Internal.Codec as Internal.Codec

-- | The address of a GitHub repository as a owner/repo pair.
type Address = { owner :: String, repo :: String }

-- | The numeric ID of an issue on GitHub. Should be paired with an Address to
-- | fully identify the location of the issue.
newtype IssueNumber = IssueNumber Int

instance Newtype IssueNumber Int
derive newtype instance Eq IssueNumber

-- | A team within a GitHub organization
type Team = { org :: String, team :: String }

-- | A member of a GitHub organization as a pair of their username (login) and
-- | their unique identifier.
type TeamMember = { login :: String, id :: Int }

-- | List members of the given team.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/teams/listMembersInOrg.md
listTeamMembers :: Octokit -> Cache -> Team -> Aff (Either GitHubError (Array TeamMember))
listTeamMembers octokit cache team = request octokit cache
  { route: GitHubRoute GET [ "orgs", team.org, "teams", team.team, "members" ] Map.empty
  , headers: Object.empty
  , args: Foreign.GitHub.noArgs
  , paginate: true
  , codec: CA.array $ CA.Record.object "TeamMember" { login: CA.string, id: CA.int }
  }

type Tag = { name :: String, sha :: String, url :: String }

-- | List repository tags
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/listTags.md
listTags :: Octokit -> Cache -> Address -> Aff (Either GitHubError (Array Tag))
listTags octokit cache address = request octokit cache
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "tags" ] Map.empty
  , headers: Object.empty
  , args: Foreign.GitHub.noArgs
  , paginate: true
  , codec: CA.array $ Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Tag"
      { name: CA.string
      , commit: CA.Record.object "Tag.Commit" { sha: CA.string, url: CA.string }
      }
  }
  where
  toJsonRep { name, sha, url } = { name, commit: { sha, url } }
  fromJsonRep { name, commit } = { name, sha: commit.sha, url: commit.url }

-- | Fetch a specific file  from the provided repository at the given ref and
-- | filepath. Filepaths should lead to a single file from the root of the repo.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/getContent.md
getContent :: Octokit -> Cache -> Address -> String -> FilePath -> Aff (Either GitHubError String)
getContent octokit cache address ref path = do
  result <- request octokit cache
    { route: GitHubRoute GET [ "repos", address.owner, address.repo, "contents", path ] (Map.singleton "ref" ref)
    , headers: Object.empty
    , args: Foreign.GitHub.noArgs
    , paginate: false
    , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Content"
        { data: CA.Record.object "Content.data"
            { type: value "file"
            , encoding: value "base64"
            , content: CA.string
            }
        }
    }
  pure (bind result (lmap DecodeError <<< Foreign.GitHub.decodeBase64Content))
  where
  value :: String -> JsonCodec String
  value expected = CA.codec'
    (\json -> CA.decode CA.string json >>= \decoded -> if decoded == expected then pure expected else Left (CA.UnexpectedValue json))
    (\_ -> CA.encode CA.string expected)

  toJsonRep (Base64Content str) = { data: { type: "file", encoding: "base64", content: str } }
  fromJsonRep { data: { content } } = Base64Content content

-- | Fetch the commit SHA for a given ref on a GitHub repository
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getRef.md
getRefCommit :: Octokit -> Cache -> Address -> String -> Aff (Either GitHubError String)
getRefCommit octokit cache address ref = request octokit cache
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "git", "ref", ref ] Map.empty
  , headers: Object.empty
  , args: Foreign.GitHub.noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Ref" { object: CA.Record.object "Ref.object" { sha: CA.string } }
  }
  where
  toJsonRep sha = { object: { sha } }
  fromJsonRep = _.object.sha

-- | Fetch the date associated with a given commit, in the RFC3339String format.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getCommit.md
getCommitDate :: Octokit -> Cache -> Address -> String -> Aff (Either GitHubError DateTime)
getCommitDate octokit cache address commitSha = request octokit cache
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "git", "commits", commitSha ] Map.empty
  , headers: Object.empty
  , args: Foreign.GitHub.noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Commit"
      { committer: CA.Record.object "Commit.committer" { date: Internal.Codec.iso8601DateTime } }
  }
  where
  toJsonRep date = { committer: { date } }
  fromJsonRep = _.committer.date

-- | Create a comment on an issue. Requires authentication.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/createComment.md
createComment :: Octokit -> Address -> IssueNumber -> String -> Aff (Either GitHubError Unit)
createComment octokit address (IssueNumber issue) body = requestWithBackoff octokit
  { route: GitHubRoute POST [ "repos", address.owner, address.repo, "issues", Int.toStringAs Int.decimal issue, "comments" ] Map.empty
  , headers: Object.empty
  , args: Foreign.GitHub.unsafeToJSArgs { body }
  , paginate: false
  , codec: CA.codec' (\_ -> pure unit) (CA.encode CA.null)
  }

-- | Close an issue. Requires authentication.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/update.md
closeIssue :: Octokit -> Address -> IssueNumber -> Aff (Either GitHubError Unit)
closeIssue octokit address (IssueNumber issue) = requestWithBackoff octokit
  { route: GitHubRoute PATCH [ "repos", address.owner, address.repo, "issues", Int.toStringAs Int.decimal issue ] Map.empty
  , headers: Object.empty
  , args: Foreign.GitHub.unsafeToJSArgs { state: "closed" }
  , paginate: false
  , codec: CA.codec' (\_ -> pure unit) (CA.encode CA.null)
  }

-- | Apply exponential backoff to requests that hang, but without cancelling
-- | requests if we have reached our rate limit and have been throttled.
-- | A helper function for implementing GET requests to the GitHub API that
-- | relies on the GitHub API to report whether there is any new data, and falls
-- | back to the cache if there is not.
request :: forall a. Octokit -> Cache -> Request a -> Aff (Either GitHubError a)
request octokit cache githubRequest@{ route: route@(GitHubRoute method _ _), codec } = do
  -- We cache GET requests, other than requests to fetch the current rate limit.
  case method of
    GET | route /= Foreign.GitHub.rateLimitRequest.route -> do
      let entryCodec = CA.Common.either Foreign.GitHub.githubErrorCodec CA.Common.json
      let entryKey = Foreign.GitHub.printGitHubRoute route
      entry <- liftEffect (Cache.readJsonEntry entryCodec entryKey cache)
      now <- liftEffect nowUTC
      case entry of
        Left _ -> do
          Console.debug $ "No cache entry for route " <> entryKey
          result <- requestWithBackoff octokit githubRequest
          liftEffect $ Cache.writeJsonEntry entryCodec entryKey (map (CA.encode codec) result) cache
          pure result

        Right cached -> case cached.value of
          Left (Foreign.GitHub.APIError err)
            -- We don't retry 404 errors because they indicate a missing resource.
            | err.statusCode == 404 -> do
                Console.debug "Cached entry is a 404 error, not retrying..."
                pure $ Left $ Foreign.GitHub.APIError err
            -- Otherwise, if we have an error in cache, we retry the request; we
            -- don't have anything usable we could return.
            | otherwise -> do
                Console.debug $ "Retrying route " <> Foreign.GitHub.printGitHubRoute route <> " because cache contains non-404 error: " <> show err
                liftEffect $ cache.remove entryKey
                request octokit cache githubRequest

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
            let _githubTime = Formatter.DateTime.format Foreign.GitHub.rfc1123Format cached.modified
            let _modifiedRequest = githubRequest { headers = Object.insert "If-Modified-Since" _githubTime githubRequest.headers }
            result <- requestWithBackoff octokit githubRequest
            case result of
              Left (Foreign.GitHub.APIError err) | err.statusCode == 304 -> do
                Console.debug $ "Received confirmation of cache validity response from GitHub, reading cache value..."
                pure result
              _ -> do
                liftEffect $ Cache.writeJsonEntry entryCodec entryKey (map (CA.encode codec) result) cache
                pure result

          Right value -> case CA.decode codec value of
            Left error -> do
              Console.debug $ "Unable to decode cache entry, returning error..."
              pure $ Left $ Foreign.GitHub.DecodeError $ CA.printJsonDecodeError error
            Right accepted ->
              pure $ Right accepted

    _ -> do
      Console.debug $ "Not a cacheable route: " <> Foreign.GitHub.printGitHubRoute route <> ", requesting..."
      requestWithBackoff octokit githubRequest

requestWithBackoff :: forall a. Octokit -> Request a -> Aff (Either GitHubError a)
requestWithBackoff octokit githubRequest = do
  Console.log $ "Making request to " <> Foreign.GitHub.printGitHubRoute githubRequest.route
  let action = Foreign.GitHub.request octokit githubRequest
  result <- withBackoff
    { delay: Duration.Milliseconds 5_000.0
    , action
    , shouldCancel: \_ -> Foreign.GitHub.request octokit Foreign.GitHub.rateLimitRequest >>= case _ of
        Right { remaining } | remaining == 0 -> pure false
        _ -> pure true
    , shouldRetry: \attempt -> if attempt <= 3 then pure (Just action) else pure Nothing
    }
  case result of
    Nothing -> pure $ Left $ Foreign.GitHub.APIError { statusCode: 400, message: "Unable to reach GitHub servers." }
    Just accepted -> pure accepted
