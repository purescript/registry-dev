module Foreign.GitHub where

import Registry.Prelude

import Affjax.StatusCode (StatusCode(..))
import Affjax.StatusCode as Http
import Control.Monad.Except as Except
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Bitraversable (ltraverse)
import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Interpolate (i)
import Data.List as List
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.RFC3339String (RFC3339String(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration as Duration
import Effect.Exception as Exception
import Effect.Now as Now
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn4, EffectFn5, EffectFn6, runEffectFn4, runEffectFn5, runEffectFn6)
import Foreign.Object as Object
import Registry.Json ((.:))
import Registry.Json as Json
import Safe.Coerce (coerce)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodePoints as Parse
import Text.Parsing.StringParser.Combinators as ParseC
import Unsafe.Coerce (unsafeCoerce)

foreign import data Octokit :: Type

foreign import mkOctokit :: String -> Effect Octokit

newtype Route = Route String

derive newtype instance Eq Route
derive newtype instance Ord Route

data GitHubError
  = APIError GitHubAPIError
  | DecodeError String

derive instance Eq GitHubError
derive instance Ord GitHubError

-- | List repository tags
-- https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/repos/listTags.md
listTags :: Octokit -> Ref GitHubCache -> { owner :: String, repo :: String } -> ExceptT GitHubError Aff (Array Tag)
listTags octokit cacheRef args = do
  result <- Except.withExceptT APIError $ requestCached paginate octokit cacheRef route Object.empty
  Except.except $ lmap DecodeError $ decodeTags result
  where
  route :: Route
  route = Route $ i "GET /repos/" args.owner "/" args.repo "/tags"

  decodeTags :: Json -> Either String (Array Tag)
  decodeTags = Json.decode >=> traverse decodeTag

  decodeTag :: Json -> Either String Tag
  decodeTag json = do
    obj <- Json.decode json
    name <- obj .: "name"
    commitObj <- obj .: "commit"
    sha <- commitObj .: "sha"
    pure { name, sha }

-- | Make a request to the GitHub API. WARNING: This function does not make use
-- | of the cache. If making a GET request, provide this as an argument to`requestCached` instead.
request :: Octokit -> Route -> Object String -> ExceptT GitHubAPIError Aff Json
request = unsafeCoerce unit

-- | Make a request to a GitHub API 'list' endpoint, paginating through all
-- | results. WARNING: This function does not make use of the cache. Provide it
-- | as an argument to `requestCached` instead.
paginate :: Octokit -> Route -> Object String -> ExceptT GitHubAPIError Aff Json
paginate = unsafeCoerce unit

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
  ]

getGitHubTime :: Effect String
getGitHubTime = do
  offset <- Now.getTimezoneOffset
  now <- Now.nowDateTime
  let adjusted = fromMaybe now $ DateTime.adjust offset now
  pure $ Formatter.DateTime.format formatRFC1123 adjusted

type GitHubCache = Map Route { modified :: String, payload :: Either GitHubAPIError Json }

requestCached
  :: (Octokit -> Route -> Object String -> ExceptT GitHubAPIError Aff Json)
  -> Octokit
  -> Ref GitHubCache
  -> Route
  -> Object String
  -> ExceptT GitHubAPIError Aff Json
requestCached runRequest octokit cacheRef route headers = do
  cache <- liftEffect $ Ref.read cacheRef
  ExceptT $ case Map.lookup route cache of
    Nothing -> do
      result <- Except.runExceptT $ runRequest octokit route headers
      modified <- liftEffect getGitHubTime
      liftEffect $ Ref.modify_ (Map.insert route { modified, payload: result }) cacheRef
      pure result

    Just cached -> case cached.payload of
      Left err
        -- We don't retry 404 errors because they indicate a missing resource.
        | err.status == StatusCode 404 -> pure cached.payload
        -- Otherwise, if we have an error in cache, we retry the request; we
        -- don't have anything usable we could return.
        | otherwise -> do
            liftEffect $ Ref.modify_ (Map.delete route) cacheRef
            Except.runExceptT $ requestCached runRequest octokit cacheRef route headers

      -- If we do have a usable cache value, then we will defer to GitHub's
      -- judgment on whether to use it or not.
      Right payload -> do
        result <- Except.runExceptT $ runRequest octokit route (Object.insert "If-Modified-Since" cached.modified headers)
        case result of
          -- 304 Not Modified means that we can rely on our local cache; nothing has
          -- changed for this resource.
          Left err | err.status == StatusCode 304 -> pure $ Right payload
          _ -> do
            modified <- liftEffect getGitHubTime
            liftEffect $ Ref.modify_ (Map.insert route { modified, payload: result }) cacheRef
            pure result

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

newtype PackageURL = PackageURL String

derive instance Newtype PackageURL _
derive newtype instance Eq PackageURL
derive newtype instance Ord PackageURL
derive newtype instance RegistryJson PackageURL

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

type GitHubAPIError =
  { status :: Http.StatusCode
  , ratelimitRemaining :: Number
  , ratelimitReset :: Instant
  , message :: String
  }

decodeGitHubAPIError :: Object Json -> Either String GitHubAPIError
decodeGitHubAPIError obj = do
  status <- map Http.StatusCode $ obj .: "status"
  response <- obj .: "response"
  responseData <- response .: "data"
  responseHeaders <- response .: "headers"
  ratelimitResetStr <- responseHeaders .: "x-ratelimit-reset"
  ratelimitReset <- note "Invalid number received for x-ratelimit-reset" $ Number.fromString ratelimitResetStr
  ratelimitRemainingStr <- responseHeaders .: "x-ratelimit-remaining"
  ratelimitRemaining <- note "Invalid number received for x-ratelimit-remaining" $ Number.fromString ratelimitRemainingStr
  message <- responseData .: "message"
  pure
    { status
    , ratelimitRemaining
    -- Instants measure milliseconds since epoch time:
    -- https://github.com/purescript/purescript-datetime/blob/a6a0cf1b0324964ad1854bc3377ed8766ba90e6f/src/Data/DateTime/Instant.purs#L20-L21
    --
    -- However, GitHub returns seconds since epoch time, so we need to multiply
    -- it by 1000 to get a valid reset time.
    , ratelimitReset: unsafeFromJust $ Instant.instant $ Duration.Milliseconds $ ratelimitReset * 1000.0
    , message
    }

convertGitHubAPIError :: forall r. Either (Object Json) r -> Aff (Either GitHubAPIError r)
convertGitHubAPIError = ltraverse \error -> case decodeGitHubAPIError error of
  Left err -> throwError $ Exception.error $ "Unexpected error decoding GitHubAPIError: " <> err
  Right value -> pure value

foreign import getReleasesImpl :: forall r. EffectFn4 Octokit Address (Object Json -> r) (Array Tag -> r) (Promise r)

getReleases :: Octokit -> Address -> ExceptT GitHubAPIError Aff (Array Tag)
getReleases octokit address = ExceptT do
  result <- Promise.toAffE $ runEffectFn4 getReleasesImpl octokit address Left Right
  convertGitHubAPIError result

foreign import getRefCommitImpl :: forall r. EffectFn5 Octokit Address String (Object Json -> r) (String -> r) (Promise r)

getRefCommit :: Octokit -> Address -> String -> ExceptT GitHubAPIError Aff String
getRefCommit octokit address ref = ExceptT do
  commitSha <- Promise.toAffE $ runEffectFn5 getRefCommitImpl octokit address ref Left Right
  convertGitHubAPIError commitSha

foreign import getCommitDateImpl :: forall r. EffectFn5 Octokit Address String (Object Json -> r) (String -> r) (Promise r)

getCommitDate :: Octokit -> Address -> String -> ExceptT GitHubAPIError Aff RFC3339String
getCommitDate octokit address sha = ExceptT do
  date <- Promise.toAffE $ runEffectFn5 getCommitDateImpl octokit address sha Left Right
  convertGitHubAPIError $ map RFC3339String date

newtype IssueNumber = IssueNumber Int

instance Newtype IssueNumber Int
derive newtype instance Eq IssueNumber
derive newtype instance Show IssueNumber
derive newtype instance RegistryJson IssueNumber

foreign import createCommentImpl :: forall r. EffectFn6 Octokit Address Int String (Object Json -> r) (Unit -> r) (Promise r)

createComment :: Octokit -> IssueNumber -> String -> ExceptT GitHubAPIError Aff Unit
createComment octokit issue body = ExceptT do
  result <- Promise.toAffE $ runEffectFn6 createCommentImpl octokit registryAddress (coerce issue) body Left Right
  convertGitHubAPIError result

foreign import closeIssueImpl :: forall r. EffectFn5 Octokit Address Int (Object Json -> r) (Unit -> r) (Promise r)

closeIssue :: Octokit -> IssueNumber -> ExceptT GitHubAPIError Aff Unit
closeIssue octokit issue = ExceptT do
  result <- Promise.toAffE $ runEffectFn5 closeIssueImpl octokit registryAddress (coerce issue) Left Right
  convertGitHubAPIError result
