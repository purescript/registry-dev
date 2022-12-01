module Foreign.GitHub
  ( Address
  , Event(..)
  , GitHubAPIError
  , GitHubError(..)
  , GitHubToken(..)
  , IssueNumber(..)
  , Octokit
  , PackageURL(..)
  , RateLimit
  , Route
  , Tag
  , Team
  , TeamMember
  , closeIssue
  , createComment
  , getCommitDate
  , getContent
  , getRateLimit
  , getRefCommit
  , listTags
  , listTeamMembers
  , mkOctokit
  , parseRepo
  , printGitHubError
  , printRateLimit
  , decodeEvent
  , githubErrorCodec
  , tagCodec
  , addressCodec
  ) where

import Registry.App.Prelude

import Affjax as Http
import Control.Monad.Except as Except
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Bitraversable (ltraverse)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CA.Common
import Data.Codec.Argonaut.Variant as CA.Variant
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Int as Int
import Data.Interpolate (i)
import Data.List as List
import Data.Newtype (over, unwrap)
import Data.Profunctor as Profunctor
import Data.String as String
import Data.String.Base64 as Base64
import Data.String.CodeUnits as String.CodeUnits
import Data.Time.Duration as Duration
import Data.Variant as Variant
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Now as Now
import Effect.Uncurried (EffectFn1, EffectFn6, runEffectFn1, runEffectFn6)
import Foreign.Object as Object
import Parsing (ParseError)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.Combinators.Array as Parsing.Combinators.Array
import Parsing.String as Parsing.String
import Parsing.String.Basic as Parsing.String.Basic
import Registry.App.Cache (Cache)
import Registry.App.Cache as Cache
import Registry.App.Json (Json)
import Registry.App.Json as Json
import Registry.Constants as Constants
import Registry.Internal.Codec as Internal.Codec
import Type.Proxy (Proxy(..))

foreign import data Octokit :: Type

newtype GitHubToken = GitHubToken String

derive instance Newtype GitHubToken _
derive newtype instance Eq GitHubToken
derive newtype instance Ord GitHubToken

foreign import mkOctokitImpl :: EffectFn1 GitHubToken Octokit

mkOctokit :: GitHubToken -> Effect Octokit
mkOctokit = runEffectFn1 mkOctokitImpl

-- | A team within a GitHub organization
type Team = { org :: String, team :: String }

-- | Member of a GitHub organization
type TeamMember = { username :: String, userId :: Int }

-- | List members of the given team
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/teams/listMembersInOrg.md
listTeamMembers :: Octokit -> Cache -> Team -> ExceptT GitHubError Aff (Array TeamMember)
listTeamMembers octokit cache { org, team } = do
  let requestArgs = { octokit, route, headers: Object.empty, args: {} }
  let cacheArgs = { cache, checkGitHub: true }
  result <- Except.withExceptT APIError $ cachedRequest uncachedPaginatedRequest requestArgs cacheArgs
  Except.except $ lmap DecodeError $ decodeTeamMembers result
  where
  route :: Route
  route = Route $ i "GET /orgs/" org "/teams/" team "/members"

  decodeTeamMembers :: Json -> Either String (Array TeamMember)
  decodeTeamMembers = Json.decodeJson (CA.array CA.json) >=> traverse decodeTeamMember

  decodeTeamMember :: Json -> Either String TeamMember
  decodeTeamMember json = do
    object <- Json.decodeJson CA.jobject json
    username <- Json.atKey "login" CA.string object
    userId <- Json.atKey "id" CA.int object
    pure { username, userId }

-- | List repository tags
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/listTags.md
listTags :: Octokit -> Cache -> Address -> ExceptT GitHubError Aff (Array Tag)
listTags octokit cache address = do
  let requestArgs = { octokit, route, headers: Object.empty, args: {} }
  let cacheArgs = { cache, checkGitHub: true }
  result <- Except.withExceptT APIError $ cachedRequest uncachedPaginatedRequest requestArgs cacheArgs
  Except.except $ lmap DecodeError $ decodeTags result
  where
  route :: Route
  route = Route $ i "GET /repos/" address.owner "/" address.repo "/tags"

  decodeTags :: Json -> Either String (Array Tag)
  decodeTags = Json.decodeJson (CA.array CA.json) >=> traverse decodeTag

  decodeTag :: Json -> Either String Tag
  decodeTag json = do
    object <- Json.decodeJson CA.jobject json
    name <- Json.atKey "name" CA.string object
    commitObject <- Json.atKey "commit" CA.jobject object
    sha <- Json.atKey "sha" CA.string commitObject
    url <- Json.atKey "url" CA.string commitObject
    pure { name, sha, url }

-- | Fetch a specific file  from the provided repository at the given ref and
-- | filepath. Filepaths should lead to a single file from the root of the repo.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/getContent.md
getContent :: Octokit -> Cache -> Address -> String -> FilePath -> ExceptT GitHubError Aff String
getContent octokit cache address ref path = do
  let requestArgs = { octokit, route, headers: Object.empty, args: {} }
  let cacheArgs = { cache, checkGitHub: false }
  result <- Except.withExceptT APIError $ cachedRequest uncachedRequest requestArgs cacheArgs
  Except.except $ lmap DecodeError $ decodeFile result
  where
  route :: Route
  route = Route $ i "GET /repos/" address.owner "/" address.repo "/contents/" path "?ref=" ref

  decodeFile :: Json -> Either String String
  decodeFile json = do
    object <- Json.decodeJson CA.jobject json
    data_ <- Json.atKey "data" CA.jobject object
    type_ <- Json.atKey "type" CA.string data_
    encoding <- Json.atKey "encoding" CA.string data_
    if encoding == "base64" && type_ == "file" then do
      contentsb64 <- Json.atKey "content" CA.string data_
      contents <- lmap Exception.message $ traverse Base64.decode $ String.split (String.Pattern "\n") contentsb64
      pure $ fold contents
    else
      Left $ "Expected file with encoding base64, but got: " <> show { encoding, type: type_ }

-- | Fetch the commit SHA for a given ref on a GitHub repository
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getRef.md
getRefCommit :: Octokit -> Cache -> Address -> String -> ExceptT GitHubError Aff String
getRefCommit octokit cache address ref = do
  let requestArgs = { octokit, route, headers: Object.empty, args: {} }
  let cacheArgs = { cache, checkGitHub: false }
  result <- Except.withExceptT APIError $ cachedRequest uncachedRequest requestArgs cacheArgs
  Except.except $ lmap DecodeError $ decodeRefSha result
  where
  route :: Route
  route = Route $ i "GET /repos/" address.owner "/" address.repo "/git/ref/" ref

  decodeRefSha :: Json -> Either String String
  decodeRefSha json = do
    object <- Json.decodeJson CA.jobject json
    innerObject <- Json.atKey "object" CA.jobject object
    Json.atKey "sha" CA.string innerObject

-- | Fetch the date associated with a given commit, in the RFC3339String format.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getCommit.md
getCommitDate :: Octokit -> Cache -> Address -> String -> ExceptT GitHubError Aff DateTime
getCommitDate octokit cache address commitSha = do
  let requestArgs = { octokit, route, headers: Object.empty, args: {} }
  let cacheArgs = { cache, checkGitHub: false }
  result <- Except.withExceptT APIError $ cachedRequest uncachedPaginatedRequest requestArgs cacheArgs
  Except.except $ lmap DecodeError $ decodeCommit result
  where
  route :: Route
  route = Route $ i "GET /repos/" address.owner "/" address.repo "/git/commits/" commitSha

  decodeCommit :: Json -> Either String DateTime
  decodeCommit json = do
    object <- Json.decodeJson CA.jobject json
    committerObject <- Json.atKey "committer" CA.jobject object
    Json.atKey "date" Internal.Codec.iso8601DateTime committerObject

-- | Create a comment on an issue in the registry repo.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/createComment.md
createComment :: Octokit -> IssueNumber -> String -> ExceptT GitHubError Aff Unit
createComment octokit issue body = do
  let args = { body }
  _ <- Except.withExceptT APIError $ uncachedRequest { octokit, route, headers: Object.empty, args }
  pure unit
  where
  route :: Route
  route = Route $ i "POST /repos/" Constants.registry.owner "/" Constants.registry.repo "/issues/" (unwrap issue) "/comments"

-- | Close an issue in the registry repo.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/update.md
closeIssue :: Octokit -> IssueNumber -> ExceptT GitHubError Aff Unit
closeIssue octokit issue = do
  let args = { state: "closed" }
  _ <- Except.withExceptT APIError $ uncachedRequest { octokit, route, headers: Object.empty, args }
  pure unit
  where
  route :: Route
  route = Route $ i "PATCH /repos/" Constants.registry.owner "/" Constants.registry.repo "/issues/" (unwrap issue)

type RateLimit =
  { limit :: Int
  , remaining :: Int
  , resetTime :: Maybe Instant
  }

-- | Get the current status of the rate limit imposed by GitHub on their API
getRateLimit :: Octokit -> ExceptT GitHubError Aff RateLimit
getRateLimit octokit = do
  result <- Except.withExceptT APIError $ uncachedRequest { octokit, route, headers: Object.empty, args: {} }
  Except.except $ lmap DecodeError $ decodeRateLimit result
  where
  route :: Route
  route = Route "GET /rate_limit"

  decodeRateLimit :: Json -> Either String RateLimit
  decodeRateLimit json = do
    object <- Json.decodeJson CA.jobject json
    dataObject <- Json.atKey "data" CA.jobject object
    resourcesObject <- Json.atKey "resources" CA.jobject dataObject
    coreObject <- Json.atKey "core" CA.jobject resourcesObject
    limit <- Json.atKey "limit" CA.int coreObject
    remaining <- Json.atKey "remaining" CA.int coreObject
    reset <- Json.atKey "reset" CA.number coreObject
    let resetTime = Instant.instant $ Duration.Milliseconds $ reset * 1000.0
    pure { limit, remaining, resetTime }

printRateLimit :: RateLimit -> Aff Unit
printRateLimit { limit, remaining, resetTime } = do
  offset <- liftEffect Now.getTimezoneOffset
  Console.log $ Array.fold
    [ "----------\n"
    , "RATE LIMIT: ("
    , show remaining
    , " of "
    , show limit
    , ")"
    , fold do
        reset <- resetTime
        let (offset' :: Duration.Minutes) = over Duration.Minutes negate offset
        dt <- DateTime.adjust offset' $ Instant.toDateTime reset
        let formatted = Formatter.DateTime.format (List.fromFoldable [ Hours12, Placeholder ":", MinutesTwoDigits, Placeholder " ", Meridiem ]) dt
        pure $ ", resetting at " <> formatted
    , "\n----------"
    ]

-- | A route for the GitHub API, ie. "GET /repos/purescript/registry/tags".
-- | Meant for internal use.
newtype Route = Route String

derive newtype instance Eq Route
derive newtype instance Ord Route

foreign import requestImpl :: forall args r. EffectFn6 Octokit Route (Object String) (Record args) (Object Json -> r) (Json -> r) (Promise r)

type UncachedRequestArgs args =
  { octokit :: Octokit
  , route :: Route
  , headers :: Object String
  , args :: Record args
  }

-- | Make a request to the GitHub API. WARNING: This function does not make use
-- | of the cache. Prefer `requestCache` when possible.
uncachedRequest :: forall args. UncachedRequestArgs args -> ExceptT GitHubAPIError Aff Json
uncachedRequest { octokit, route, headers, args } = ExceptT do
  let request = runEffectFn6 requestImpl octokit route headers args Left Right
  rateLimitBackoff octokit (Promise.toAffE request)

foreign import paginateImpl :: forall args r. EffectFn6 Octokit Route (Object String) (Record args) (Object Json -> r) (Json -> r) (Promise r)

-- | Make a request to the GitHub API, paginating through allresults. WARNING:
-- | This function should only be used on the GitHub 'list' endpoints.
uncachedPaginatedRequest :: forall args. UncachedRequestArgs args -> ExceptT GitHubAPIError Aff Json
uncachedPaginatedRequest { octokit, route, headers, args } = ExceptT do
  let request = runEffectFn6 paginateImpl octokit route headers args Left Right
  rateLimitBackoff octokit (Promise.toAffE request)

-- | Apply exponential backoff to requests that hang, but without cancelling
-- | requests if we have reached our rate limit and have been throttled.
rateLimitBackoff :: Octokit -> Aff (Either (Object Json) Json) -> Aff (Either GitHubAPIError Json)
rateLimitBackoff octokit action = do
  maybeResult <- withBackoff
    { delay: Aff.Milliseconds 5_000.0
    , action
    , shouldCancel: \_ -> Except.runExceptT (getRateLimit octokit) >>= case _ of
        Right { remaining } | remaining == 0 -> pure false
        _ -> pure true
    , shouldRetry: \attempt -> if attempt <= 3 then pure (Just action) else pure Nothing
    }
  case maybeResult of
    Nothing -> pure $ Left { statusCode: 400, message: "Unable to reach GitHub servers." }
    Just result -> convertGitHubAPIError result

-- GitHub uses the RFC1123 time format: "Thu, 05 Jul 2022"
-- http://www.csgnetwork.com/timerfc1123calc.html
--
-- It expects the time to be in UTC.
formatRFC1123 :: Formatter
formatRFC1123 = List.fromFoldable
  [ DayOfWeekNameShort
  , Placeholder ", "
  , DayOfMonthTwoDigits
  , Placeholder " "
  , MonthShort
  , Placeholder " "
  , YearFull
  , Placeholder " "
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  , Placeholder " "
  , Placeholder "UTC"
  ]

-- | A helper function for implementing GET requests to the GitHub API that
-- | relies on the GitHub API to report whether there is any new data, and falls
-- | back to the cache if there is not. Should only be used on GET requests; for
-- | POST requests, use `request`.
cachedRequest
  :: forall args
   . (UncachedRequestArgs args -> ExceptT GitHubAPIError Aff Json)
  -> UncachedRequestArgs args
  -> { cache :: Cache, checkGitHub :: Boolean }
  -> ExceptT GitHubAPIError Aff Json
cachedRequest runRequest requestArgs@{ route: Route route } { cache, checkGitHub } = do
  let codec = CA.Common.either githubApiErrorCodec CA.json
  entry <- liftEffect (Cache.readJsonEntry codec route cache)
  now <- liftEffect nowUTC
  ExceptT $ case entry of
    Left _ -> do
      Console.log $ "CACHE MISS: Malformed or no entry for " <> route
      result <- Except.runExceptT $ runRequest requestArgs
      liftEffect $ Cache.writeJsonEntry codec route result cache
      pure result

    Right cached -> case cached.value of
      Left err
        -- We don't retry 404 errors because they indicate a missing resource.
        | err.statusCode == 404 -> pure $ Left err
        -- Otherwise, if we have an error in cache, we retry the request; we
        -- don't have anything usable we could return.
        | otherwise -> do
            Console.log $ "CACHE ERROR: Deleting non-404 error entry and retrying " <> route
            Console.log $ show err
            liftEffect $ cache.remove route
            Except.runExceptT $ cachedRequest runRequest requestArgs { cache, checkGitHub }

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
      Right payload
        | checkGitHub, DateTime.diff now cached.modified >= Duration.Hours 1.0 -> do
            Console.log $ "CACHE EXPIRED: " <> route
            let _gitHubTime = Formatter.DateTime.format formatRFC1123 cached.modified
            result <- Except.runExceptT $ runRequest $ requestArgs
            {- TODO: Re-enable when GitHub honors requests again.
            result <- Except.runExceptT $ runRequest $ requestArgs
              { headers = Object.insert "If-Modified-Since" gitHubTime requestArgs.headers }
            -}
            case result of
              -- A 304 response means the resource has not changed and we should
              -- return from cache.
              Left err | err.statusCode == 304 -> do
                pure $ Right payload
              _ -> do
                liftEffect $ Cache.writeJsonEntry codec route result cache
                pure result
        | otherwise ->
            pure $ Right payload

newtype IssueNumber = IssueNumber Int

instance Newtype IssueNumber Int
derive newtype instance Eq IssueNumber

newtype PackageURL = PackageURL String

derive instance Newtype PackageURL _
derive newtype instance Eq PackageURL
derive newtype instance Ord PackageURL

newtype Event = Event
  { issueNumber :: IssueNumber
  , body :: String
  , username :: String
  }

derive instance Newtype Event _

decodeEvent :: Json -> Either String Event
decodeEvent json = do
  object <- Json.decodeJson CA.jobject json
  username <- Json.atKey "login" CA.string =<< Json.atKey "sender" CA.jobject object

  issueObject <- Json.atKey "issue" CA.jobject object
  issueNumber <- Json.atKey "number" CA.int issueObject

  -- We accept issue creation and issue comment events, but both contain an
  -- 'issue' field. However, only comments contain a 'comment' field. For that
  -- reason we first try to parse the comment and fall back to the issue if
  -- that fails.
  body <- Json.atKey "body" CA.string =<< Json.atKey "comment" CA.jobject object <|> pure issueObject
  pure $ Event { body, username, issueNumber: IssueNumber issueNumber }

type Address = { owner :: String, repo :: String }

addressCodec :: JsonCodec Address
addressCodec = Json.object "Address"
  { owner: CA.string
  , repo: CA.string
  }

type Tag = { name :: String, sha :: String, url :: Http.URL }

tagCodec :: JsonCodec Tag
tagCodec = Json.object "Tag"
  { name: CA.string
  , sha: CA.string
  , url: CA.string
  }

parseRepo :: PackageURL -> Either ParseError Address
parseRepo (PackageURL input) = Parsing.runParser input do
  _ <- Parsing.Combinators.choice
    [ Parsing.String.string "https://github.com/"
    , Parsing.String.string "git://github.com/"
    , Parsing.String.string "git@github.com/"
    ]

  owner <- do
    let
      ownerChoice = Parsing.Combinators.choice
        [ Parsing.String.Basic.alphaNum
        , Parsing.String.char '-'
        ]
    Tuple chars _ <- Parsing.Combinators.Array.manyTill_ ownerChoice (Parsing.String.char '/')
    pure $ String.CodeUnits.fromCharArray chars

  repoWithSuffix <- String.CodeUnits.fromCharArray <$> Array.many Parsing.String.anyChar
  let repo = fromMaybe repoWithSuffix (String.stripSuffix (String.Pattern ".git") repoWithSuffix)

  pure { owner, repo }

data GitHubError
  = APIError GitHubAPIError
  | DecodeError String

derive instance Eq GitHubError
derive instance Ord GitHubError

githubErrorCodec :: JsonCodec GitHubError
githubErrorCodec = Profunctor.dimap toVariant fromVariant $ CA.Variant.variantMatch
  { apiError: Right githubApiErrorCodec
  , decodeError: Right CA.string
  }
  where
  toVariant = case _ of
    APIError error -> Variant.inj (Proxy :: _ "apiError") error
    DecodeError error -> Variant.inj (Proxy :: _ "decodeError") error

  fromVariant = Variant.match
    { apiError: APIError
    , decodeError: DecodeError
    }

printGitHubError :: GitHubError -> String
printGitHubError = case _ of
  APIError fields -> Array.fold
    [ "GitHub API error ("
    , Int.toStringAs Int.decimal fields.statusCode
    , "): "
    , fields.message
    ]
  DecodeError err -> Array.fold
    [ "Decoding error: "
    , err
    ]

type GitHubAPIError =
  { statusCode :: Int
  , message :: String
  }

githubApiErrorCodec :: JsonCodec GitHubAPIError
githubApiErrorCodec = Json.object "GitHubAPIError"
  { statusCode: CA.int
  , message: CA.string
  }

convertGitHubAPIError :: forall r. Either (Object Json) r -> Aff (Either GitHubAPIError r)
convertGitHubAPIError = ltraverse \error -> case decodeGitHubAPIError error of
  Left err -> throwError $ Exception.error $ "Unexpected error decoding GitHubAPIError: " <> err
  Right value -> pure value
  where
  decodeGitHubAPIError :: Object Json -> Either String GitHubAPIError
  decodeGitHubAPIError object = do
    statusCode <- Json.atKey "status" CA.int object
    message <- case statusCode of
      304 -> pure ""
      _ ->
        Json.atKey "response" CA.jobject object
          >>= Json.atKey "data" CA.jobject
          >>= Json.atKey "message" CA.string

    pure { statusCode, message }
