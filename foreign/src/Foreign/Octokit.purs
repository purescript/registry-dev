-- | Low-level bindings to Octokit and its request functions.
-- |
-- | There is a GitHub module in the registry application as well, containing
-- | app-specific requests, but the modules are kept separate because this low-
-- | level code is required to implement both the app-specific requests and the
-- | logging effect that those requests rely on.
module Registry.Foreign.Octokit
  ( Address
  , Base64Content(..)
  , GitHubAPIError
  , GitHubError(..)
  , GitHubRoute(..)
  , GitHubToken(..)
  , IssueNumber(..)
  , JSArgs(..)
  , Octokit
  , RateLimit
  , Request
  , Tag
  , Team
  , TeamMember
  , closeIssueRequest
  , createCommentRequest
  , decodeBase64Content
  , getCommitDateRequest
  , getContentRequest
  , getRefCommitRequest
  , githubApiErrorCodec
  , githubErrorCodec
  , listTagsRequest
  , listTeamMembersRequest
  , newOctokit
  , noArgs
  , printGitHubError
  , printGitHubRoute
  , rateLimitRequest
  , request
  , unsafeToJSArgs
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Codec.Argonaut.Variant as CA.Variant
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor as Profunctor
import Data.String as String
import Data.String.Base64 as Base64
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, EffectFn6, runEffectFn2, runEffectFn6)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Path (FilePath)
import Registry.Internal.Codec as Internal.Codec
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A private GitHub API token
newtype GitHubToken = GitHubToken String

derive instance Newtype GitHubToken _
derive newtype instance Eq GitHubToken
derive newtype instance Ord GitHubToken

-- | An instance of GitHub's Octokit client
foreign import data Octokit :: Type

foreign import newOctokitImpl :: EffectFn2 GitHubToken String Octokit

newOctokit :: forall m. MonadEffect m => GitHubToken -> String -> m Octokit
newOctokit token baseUrl = liftEffect $ runEffectFn2 newOctokitImpl token baseUrl

-- | A newline-delimited base64-encoded file retrieved from the GitHub API
newtype Base64Content = Base64Content String

derive instance Newtype Base64Content _

decodeBase64Content :: Base64Content -> Either String String
decodeBase64Content (Base64Content string) =
  case traverse Base64.decode $ String.split (String.Pattern "\n") string of
    Left error -> Left $ Aff.message error
    Right values -> Right $ Array.fold values

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
listTeamMembersRequest :: Team -> Request (Array TeamMember)
listTeamMembersRequest team =
  { route: GitHubRoute GET [ "orgs", team.org, "teams", team.team, "members" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: true
  , codec: CA.array $ CA.Record.object "TeamMember" { login: CA.string, id: CA.int }
  }

type Tag = { name :: String, sha :: String, url :: String }

-- | List repository tags
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/listTags.md
listTagsRequest :: Address -> Request (Array Tag)
listTagsRequest address =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "tags" ] Map.empty
  , headers: Object.empty
  , args: noArgs
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
getContentRequest :: { address :: Address, ref :: String, path :: FilePath } -> Request Base64Content
getContentRequest { address, ref, path } =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "contents", path ] (Map.singleton "ref" ref)
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Content"
      { data: CA.Record.object "Content.data"
          { type: value "file"
          , encoding: value "base64"
          , content: CA.string
          }
      }
  }
  where
  value :: String -> JsonCodec String
  value expected = CA.codec'
    (\json -> CA.decode CA.string json >>= \decoded -> if decoded == expected then pure expected else Left (CA.UnexpectedValue json))
    (\_ -> CA.encode CA.string expected)

  toJsonRep (Base64Content str) = { data: { type: "file", encoding: "base64", content: str } }
  fromJsonRep { data: { content } } = Base64Content content

-- | Fetch the commit SHA for a given ref on a GitHub repository
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getRef.md
getRefCommitRequest :: { address :: Address, ref :: String } -> Request String
getRefCommitRequest { address, ref } =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "git", "ref", ref ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Ref" { object: CA.Record.object "Ref.object" { sha: CA.string } }
  }
  where
  toJsonRep sha = { object: { sha } }
  fromJsonRep = _.object.sha

-- | Fetch the date associated with a given commit, in the RFC3339String format.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getCommit.md
getCommitDateRequest :: { address :: Address, commitSha :: String } -> Request DateTime
getCommitDateRequest { address, commitSha } =
  { route: GitHubRoute GET [ "repos", address.owner, address.repo, "git", "commits", commitSha ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "CommitData"
      { data: CA.Record.object "Commit"
          { committer: CA.Record.object "Commit.committer"
              { date: Internal.Codec.iso8601DateTime
              }
          }
      }
  }
  where
  toJsonRep date = { data: { committer: { date } } }
  fromJsonRep = _.data.committer.date

-- | Create a comment on an issue. Requires authentication.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/createComment.md
createCommentRequest :: { address :: Address, issue :: IssueNumber, body :: String } -> Request Unit
createCommentRequest { address, issue: IssueNumber issue, body } =
  { route: GitHubRoute POST [ "repos", address.owner, address.repo, "issues", Int.toStringAs Int.decimal issue, "comments" ] Map.empty
  , headers: Object.empty
  , args: unsafeToJSArgs { body }
  , paginate: false
  , codec: CA.codec' (\_ -> pure unit) (CA.encode CA.null)
  }

-- | Close an issue. Requires authentication.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/update.md
closeIssueRequest :: { address :: Address, issue :: IssueNumber } -> Request Unit
closeIssueRequest { address, issue: IssueNumber issue } =
  { route: GitHubRoute PATCH [ "repos", address.owner, address.repo, "issues", Int.toStringAs Int.decimal issue ] Map.empty
  , headers: Object.empty
  , args: unsafeToJSArgs { state: "closed" }
  , paginate: false
  , codec: CA.codec' (\_ -> pure unit) (CA.encode CA.null)
  }

type RateLimit =
  { limit :: Int
  , remaining :: Int
  , resetTime :: Maybe Instant
  }

rateLimitRequest :: Request RateLimit
rateLimitRequest =
  { route: GitHubRoute GET [ "rate_limit" ] Map.empty
  , headers: Object.empty
  , args: noArgs
  , paginate: false
  , codec: Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "RateLimit"
      { data: CA.Record.object "RateLimit.data"
          { resources: CA.Record.object "RateLimit.data.resources"
              { core: CA.Record.object "RateLimit.data.resources.core"
                  { limit: CA.int
                  , remaining: CA.int
                  , reset: CA.number
                  }
              }
          }
      }
  }
  where
  toJsonRep { limit, remaining, resetTime } = do
    let reset = Maybe.fromMaybe (-9999.0) ((unwrap <<< Instant.unInstant) <$> resetTime)
    { data: { resources: { core: { limit, remaining, reset } } } }

  fromJsonRep { data: { resources: { core: { limit, remaining, reset } } } } =
    { limit, remaining, resetTime: Instant.instant $ Aff.Milliseconds $ reset * 1000.0 }

-- | A route for the GitHub API, ie. "GET /repos/purescript/registry/tags".
-- | Meant for internal use.
data GitHubRoute = GitHubRoute Method (Array String) (Map String String)

derive instance Eq GitHubRoute
derive instance Ord GitHubRoute

-- | Format a route as a usable GitHub route for Octokit
printGitHubRoute :: GitHubRoute -> String
printGitHubRoute (GitHubRoute method segments params) = show method <> " " <> printPath <> printParams
  where
  printPath = Array.foldMap (append "/") segments
  printParams = case Map.size params of
    0 -> ""
    _ -> append "?" $ String.joinWith "&" $ map (\(Tuple key val) -> key <> "=" <> val) $ Map.toUnfoldable params

-- | An opaque type for PureScript types we want to pass directly to JavaScript
-- | through the FFI. Should only be used with JavaScript-compatible types for
-- | the sake of setting headers.
data JSArgs

-- | Coerce a record to a JSArgs opaque type.
unsafeToJSArgs :: forall a. Record a -> JSArgs
unsafeToJSArgs = unsafeCoerce

noArgs :: JSArgs
noArgs = unsafeToJSArgs {}

type Request a =
  { route :: GitHubRoute
  , headers :: Object String
  , args :: JSArgs
  , paginate :: Boolean
  , codec :: JsonCodec a
  }

foreign import requestImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object Json -> r) (Json -> r) (Promise r)
foreign import paginateImpl :: forall r. EffectFn6 Octokit String (Object String) JSArgs (Object Json -> r) (Json -> r) (Promise r)

-- | Make a request to the GitHub API
--
-- TODO: We ought to pull off the 'etag' from the response headers, because we
-- can then send it with a 'If-None-Match' header for conditional requests.
request :: forall m a. MonadAff m => Octokit -> Request a -> m (Either GitHubError a)
request octokit { route, headers, args, paginate, codec } = do
  result <- liftAff $ Promise.toAffE $ runEffectFn6 (if paginate then paginateImpl else requestImpl) octokit (printGitHubRoute route) headers args Left Right
  pure $ case result of
    Left githubError -> case decodeGitHubAPIError githubError of
      Left decodeError -> Left $ UnexpectedError decodeError
      Right decoded -> Left $ APIError decoded
    Right json -> case CA.decode codec json of
      Left decodeError -> Left $ DecodeError $ CA.printJsonDecodeError decodeError
      Right parsed -> Right parsed
  where
  decodeGitHubAPIError :: Object Json -> Either String GitHubAPIError
  decodeGitHubAPIError object = lmap CA.printJsonDecodeError do
    statusCode <- atKey "status" CA.int object
    message <- case statusCode of
      304 -> pure ""
      _ -> atKey "response" CA.jobject object >>= atKey "data" CA.jobject >>= atKey "message" CA.string
    pure { statusCode, message }

type GitHubAPIError =
  { statusCode :: Int
  , message :: String
  }

githubApiErrorCodec :: JsonCodec GitHubAPIError
githubApiErrorCodec = CA.Record.object "GitHubAPIError"
  { statusCode: CA.int
  , message: CA.string
  }

data GitHubError
  = UnexpectedError String
  | APIError GitHubAPIError
  | DecodeError String

derive instance Eq GitHubError
derive instance Ord GitHubError

githubErrorCodec :: JsonCodec GitHubError
githubErrorCodec = Profunctor.dimap toVariant fromVariant $ CA.Variant.variantMatch
  { unexpectedError: Right CA.string
  , apiError: Right githubApiErrorCodec
  , decodeError: Right CA.string
  }
  where
  toVariant = case _ of
    UnexpectedError error -> Variant.inj (Proxy :: _ "unexpectedError") error
    APIError error -> Variant.inj (Proxy :: _ "apiError") error
    DecodeError error -> Variant.inj (Proxy :: _ "decodeError") error

  fromVariant = Variant.match
    { unexpectedError: UnexpectedError
    , apiError: APIError
    , decodeError: DecodeError
    }

printGitHubError :: GitHubError -> String
printGitHubError = case _ of
  UnexpectedError message -> Array.fold
    [ "Unexpected error: "
    , message
    ]
  APIError fields -> Array.fold
    [ "GitHub API error ("
    , Int.toStringAs Int.decimal fields.statusCode
    , "): "
    , fields.message
    ]
  DecodeError error -> Array.fold
    [ "Decoding error: "
    , error
    ]

atKey :: forall a. String -> JsonCodec a -> Object Json -> Either JsonDecodeError a
atKey key codec object =
  Maybe.maybe
    (Left (CA.AtKey key CA.MissingValue))
    (lmap (CA.AtKey key) <<< CA.decode codec)
    (Object.lookup key object)
