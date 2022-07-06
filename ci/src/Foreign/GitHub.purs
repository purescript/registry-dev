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
  , closeIssue
  , createComment
  , getCommitDate
  , getContent
  , getRateLimit
  , getRefCommit
  , listTags
  , mkOctokit
  , parseRepo
  , printGitHubError
  , printRateLimit
  , registryAddress
  ) where

import Registry.Prelude

import Control.Monad.Except as Except
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Bitraversable (ltraverse)
import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Int as Int
import Data.Interpolate (i)
import Data.List as List
import Data.Newtype (over, unwrap)
import Data.RFC3339String (RFC3339String(..))
import Data.String as String
import Data.String.Base64 as Base64
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration as Duration
import Effect.Exception as Aff
import Effect.Exception as Exception
import Effect.Now as Now
import Effect.Uncurried (EffectFn1, EffectFn6, runEffectFn1, runEffectFn6)
import Foreign.Object as Object
import Registry.Cache (Cache)
import Registry.Cache as Cache
import Registry.Json ((.:))
import Registry.Json as Json
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodePoints as Parse
import Text.Parsing.StringParser.Combinators as ParseC

foreign import data Octokit :: Type

newtype GitHubToken = GitHubToken String

derive instance Newtype GitHubToken _
derive newtype instance Eq GitHubToken
derive newtype instance Ord GitHubToken

foreign import mkOctokitImpl :: EffectFn1 GitHubToken Octokit

mkOctokit :: GitHubToken -> Effect Octokit
mkOctokit = runEffectFn1 mkOctokitImpl

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
  decodeTags = Json.decode >=> traverse decodeTag

  decodeTag :: Json -> Either String Tag
  decodeTag json = do
    obj <- Json.decode json
    name <- obj .: "name"
    commitObj <- obj .: "commit"
    sha <- commitObj .: "sha"
    pure { name, sha }

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
    obj <- Json.decode json
    _data <- obj .: "data"
    _type <- _data .: "type"
    encoding <- _data .: "encoding"
    if encoding == "base64" && _type == "file" then do
      contentsb64 <- _data .: "content"
      contents <- lmap Aff.message $ traverse Base64.decode $ String.split (String.Pattern "\n") contentsb64
      pure $ fold contents
    else
      Left $ "Expected file with encoding base64, but got: " <> show { encoding, type: _type }

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
  decodeRefSha = Json.decode >=> (_ .: "object") >=> (_ .: "sha")

-- | Fetch the date associated with a given commit, in the RFC3339String format.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getCommit.md
getCommitDate :: Octokit -> Cache -> Address -> String -> ExceptT GitHubError Aff RFC3339String
getCommitDate octokit cache address commitSha = do
  let requestArgs = { octokit, route, headers: Object.empty, args: {} }
  let cacheArgs = { cache, checkGitHub: false }
  result <- Except.withExceptT APIError $ cachedRequest uncachedPaginatedRequest requestArgs cacheArgs
  Except.except $ lmap DecodeError $ decodeCommit result
  where
  route :: Route
  route = Route $ i "GET /repos/" address.owner "/" address.repo "/git/commits/" commitSha

  decodeCommit :: Json -> Either String RFC3339String
  decodeCommit json = do
    obj <- Json.decode json
    committer <- obj .: "committer"
    date <- committer .: "date"
    pure $ RFC3339String date

-- | Create a comment on an issue in the registry repo.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/createComment.md
createComment :: Octokit -> IssueNumber -> String -> ExceptT GitHubError Aff Unit
createComment octokit issue body = do
  let args = { body }
  _ <- Except.withExceptT APIError $ uncachedRequest { octokit, route, headers: Object.empty, args }
  pure unit
  where
  route :: Route
  route = Route $ i "POST /repos/" registryAddress.owner "/" registryAddress.repo "/issues/" (unwrap issue) "/comments"

-- | Close an issue in the registry repo.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/update.md
closeIssue :: Octokit -> IssueNumber -> ExceptT GitHubError Aff Unit
closeIssue octokit issue = do
  let args = { status: "closed " }
  _ <- Except.withExceptT APIError $ uncachedRequest { octokit, route, headers: Object.empty, args }
  pure unit
  where
  route :: Route
  route = Route $ i "PATCH /repos/" registryAddress.owner "/" registryAddress.repo "/issues/" (unwrap issue)

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
    obj <- Json.decode json
    rateObj <- (_ .: "core") =<< (_ .: "resources") =<< obj .: "data"
    limit <- rateObj .: "limit"
    remaining <- rateObj .: "remaining"
    reset <- rateObj .: "reset"
    let resetTime = Instant.instant $ Duration.Milliseconds $ reset * 1000.0
    pure { limit, remaining, resetTime }

printRateLimit :: RateLimit -> Aff Unit
printRateLimit { limit, remaining, resetTime } = do
  offset <- liftEffect Now.getTimezoneOffset
  log $ Array.fold
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
derive newtype instance Json.StringEncodable Route

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
  result <- Promise.toAffE $ runEffectFn6 requestImpl octokit route headers args Left Right
  convertGitHubAPIError result

foreign import paginateImpl :: forall args r. EffectFn6 Octokit Route (Object String) (Record args) (Object Json -> r) (Json -> r) (Promise r)

-- | Make a request to the GitHub API, paginating through allresults. WARNING:
-- | This function should only be used on the GitHub 'list' endpoints.
uncachedPaginatedRequest :: forall args. UncachedRequestArgs args -> ExceptT GitHubAPIError Aff Json
uncachedPaginatedRequest { octokit, route, headers, args } = ExceptT do
  result <- Promise.toAffE $ runEffectFn6 paginateImpl octokit route headers args Left Right
  convertGitHubAPIError result

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

toGitHubTime :: DateTime.DateTime -> String
toGitHubTime = Formatter.DateTime.format formatRFC1123

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
  entry <- liftEffect (Cache.readJsonEntry route cache)
  now <- liftEffect Cache.nowUTC
  ExceptT $ case entry of
    Left err -> do
      log $ "CACHE MISS: Malformed or no entry for " <> route
      log err
      result <- Except.runExceptT $ runRequest requestArgs
      liftEffect $ Cache.writeJsonEntry route result cache
      pure result

    Right cached -> case cached.value of
      Left err
        -- We don't retry 404 errors because they indicate a missing resource.
        | err.statusCode == 404 -> pure $ Left err
        -- Otherwise, if we have an error in cache, we retry the request; we
        -- don't have anything usable we could return.
        | otherwise -> do
            log $ "CACHE ERROR: Deleting non-404 error entry and retrying " <> route
            logShow err
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
        | checkGitHub, DateTime.diff now cached.modified >= Duration.Hours 4.0 -> do
            log $ "CACHE EXPIRED: " <> route
            result <- Except.runExceptT $ runRequest $ requestArgs
              { headers = Object.insert "If-Modified-Since" (toGitHubTime cached.modified) requestArgs.headers }
            case result of
              -- A 304 response means the resource has not changed and we should
              -- return from cache.
              Left err | err.statusCode == 304 -> do
                -- TODO: Remove this line when GitHub starts honoring rate limit
                -- exclusions for conditional requests again.
                liftEffect $ Cache.writeJsonEntry route payload cache
                pure $ Right payload
              _ -> do
                liftEffect $ Cache.writeJsonEntry route result cache
                pure result
        | otherwise ->
            pure $ Right payload

newtype IssueNumber = IssueNumber Int

instance Newtype IssueNumber Int
derive newtype instance Eq IssueNumber
derive newtype instance Show IssueNumber
derive newtype instance RegistryJson IssueNumber

newtype PackageURL = PackageURL String

derive instance Newtype PackageURL _
derive newtype instance Eq PackageURL
derive newtype instance Ord PackageURL
derive newtype instance RegistryJson PackageURL

newtype Event = Event
  { issueNumber :: IssueNumber
  , body :: String
  }

derive instance Newtype Event _

instance RegistryJson Event where
  encode (Event fields) = Json.encode fields
  decode json = do
    obj <- Json.decode json
    issue <- obj .: "issue"
    issueNumber <- issue .: "number"
    -- We accept issue creation and issue comment events, but both contain an
    -- 'issue' field. However, only comments contain a 'comment' field. For that
    -- reason we first try to parse the comment and fall back to the issue if
    -- that fails.
    body <- (_ .: "body") =<< obj .: "comment" <|> pure issue
    pure $ Event { body, issueNumber: IssueNumber issueNumber }

type Address = { owner :: String, repo :: String }

registryAddress :: Address
registryAddress = { owner: "purescript", repo: "registry" }

type Tag = { name :: String, sha :: String }

parseRepo :: PackageURL -> Either Parser.ParseError Address
parseRepo = unwrap >>> Parser.runParser do
  void $ Parse.string "https://github.com/"
    <|> Parse.string "git://github.com/"
    <|> Parse.string "git@github.com:"
  owner <- map (fromCharArray <<< List.toUnfoldable)
    $ ParseC.manyTill (ParseC.choice [ Parse.alphaNum, Parse.char '-' ]) (Parse.char '/')
  repoWithSuffix <- map (fromCharArray <<< List.toUnfoldable)
    $ ParseC.many Parse.anyChar
  let repo = fromMaybe repoWithSuffix $ String.stripSuffix (String.Pattern ".git") repoWithSuffix
  pure { owner, repo }

data GitHubError
  = APIError GitHubAPIError
  | DecodeError String

derive instance Eq GitHubError
derive instance Ord GitHubError

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

convertGitHubAPIError :: forall r. Either (Object Json) r -> Aff (Either GitHubAPIError r)
convertGitHubAPIError = ltraverse \error -> case decodeGitHubAPIError error of
  Left err -> throwError $ Exception.error $ "Unexpected error decoding GitHubAPIError: " <> err
  Right value -> pure value
  where
  decodeGitHubAPIError :: Object Json -> Either String GitHubAPIError
  decodeGitHubAPIError obj = do
    statusCode <- obj .: "status"
    message <- case statusCode of
      304 -> pure ""
      _ -> (_ .: "message") =<< (_ .: "data") =<< obj .: "response"
    pure { statusCode, message }
