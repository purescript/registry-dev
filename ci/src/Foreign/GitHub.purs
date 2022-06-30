module Foreign.GitHub
  ( Address
  , Event(..)
  , GitHubAPIError
  , GitHubCache
  , GitHubError(..)
  , GitHubToken(..)
  , IssueNumber(..)
  , Octokit
  , PackageURL(..)
  , Route
  , Tag
  , cacheName
  , closeIssue
  , createComment
  , getCommitDate
  , getContent
  , getRefCommit
  , listTags
  , mkOctokit
  , parseRepo
  , printGitHubError
  , readGitHubCache
  , registryAddress
  , writeGitHubCache
  ) where

import Registry.Prelude

import Control.Monad.Except as Except
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Array as Array
import Data.Bitraversable (ltraverse)
import Data.DateTime as DateTime
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.Formatter.DateTime as Formatter.DateTime
import Data.Int as Int
import Data.Interpolate (i)
import Data.List as List
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.RFC3339String (RFC3339String(..))
import Data.String as String
import Data.String.Base64 as Base64
import Data.String.CodeUnits (fromCharArray)
import Effect.Exception as Aff
import Effect.Exception as Exception
import Effect.Now as Now
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn6, runEffectFn1, runEffectFn6)
import Foreign.Object as Object
import Node.Path as Path
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
listTags :: Octokit -> Ref GitHubCache -> Address -> ExceptT GitHubError Aff (Array Tag)
listTags octokit cacheRef address = do
  result <- Except.withExceptT APIError $ requestCached paginate octokit cacheRef route Object.empty {} true
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
getContent :: Octokit -> Ref GitHubCache -> Address -> String -> FilePath -> ExceptT GitHubError Aff String
getContent octokit cacheRef address ref path = do
  result <- Except.withExceptT APIError $ requestCached request octokit cacheRef route Object.empty {} false
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
getRefCommit :: Octokit -> Ref GitHubCache -> Address -> String -> ExceptT GitHubError Aff String
getRefCommit octokit cacheRef address ref = do
  result <- Except.withExceptT APIError $ requestCached paginate octokit cacheRef route Object.empty {} false
  Except.except $ lmap DecodeError $ decodeRefSha result
  where
  route :: Route
  route = Route $ i "GET /repos/" address.owner "/" address.repo "/git/ref/" ref

  decodeRefSha :: Json -> Either String String
  decodeRefSha = Json.decode >=> (_ .: "object") >=> (_ .: "sha")

-- | Fetch the date associated with a given commit, in the RFC3339String format.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/git/getCommit.md
getCommitDate :: Octokit -> Ref GitHubCache -> Address -> String -> ExceptT GitHubError Aff RFC3339String
getCommitDate octokit cacheRef address commitSha = do
  result <- Except.withExceptT APIError $ requestCached paginate octokit cacheRef route Object.empty {} false
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
  _ <- Except.withExceptT APIError $ request octokit route Object.empty { body }
  pure unit
  where
  route :: Route
  route = Route $ i "POST /repos/" registryAddress.owner "/" registryAddress.repo "/issues/" (unwrap issue) "/comments"

-- | Close an issue in the registry repo.
-- | https://github.com/octokit/plugin-rest-endpoint-methods.js/blob/v5.16.0/docs/issues/update.md
closeIssue :: Octokit -> IssueNumber -> ExceptT GitHubError Aff Unit
closeIssue octokit issue = do
  _ <- Except.withExceptT APIError $ request octokit route Object.empty { status: "closed" }
  pure unit
  where
  route :: Route
  route = Route $ i "PATCH /repos/" registryAddress.owner "/" registryAddress.repo "/issues/" (unwrap issue)

-- | A route for the GitHub API, ie. "GET /repos/purescript/registry/tags".
-- | Meant for internal use.
newtype Route = Route String

derive newtype instance Eq Route
derive newtype instance Ord Route
derive newtype instance Json.StringEncodable Route

foreign import requestImpl :: forall args r. EffectFn6 Octokit Route (Object String) (Record args) (Object Json -> r) (Json -> r) (Promise r)

-- | Make a request to the GitHub API. WARNING: This function does not make use
-- | of the cache. Prefer `requestCache` when possible.
request :: forall args. Octokit -> Route -> Object String -> Record args -> ExceptT GitHubAPIError Aff Json
request octokit route headers args = ExceptT do
  result <- Promise.toAffE $ runEffectFn6 requestImpl octokit route headers args Left Right
  convertGitHubAPIError result

foreign import paginateImpl :: forall args r. EffectFn6 Octokit Route (Object String) (Record args) (Object Json -> r) (Json -> r) (Promise r)

-- | Make a request to the GitHub API, paginating through allresults. WARNING:
-- | This function should only be used on the GitHub 'list' endpoints.
paginate :: forall args. Octokit -> Route -> Object String -> Record args -> ExceptT GitHubAPIError Aff Json
paginate octokit route headers args = ExceptT do
  result <- Promise.toAffE $ runEffectFn6 paginateImpl octokit route headers args Left Right
  convertGitHubAPIError result

-- | A cache of requests and responses to the GitHub API, used by the
-- | `requestCached` function to minimize the occurence of rate-limiting.
type GitHubCache = Map Route { modified :: String, payload :: Either GitHubAPIError Json }

cacheName :: String
cacheName = "github-cache.json"

writeGitHubCache :: GitHubCache -> Aff Unit
writeGitHubCache cache = Json.writeJsonFile (Path.concat [ ".cache", cacheName ]) cache

readGitHubCache :: Aff (Either String GitHubCache)
readGitHubCache = Json.readJsonFile (Path.concat [ ".cache", cacheName ])

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

getGitHubTime :: Effect String
getGitHubTime = do
  offset <- Now.getTimezoneOffset
  now <- Now.nowDateTime
  let adjusted = fromMaybe now $ DateTime.adjust offset now
  pure $ Formatter.DateTime.format formatRFC1123 adjusted

-- | A helper function for implementing GET requests to the GitHub API that
-- | relies on the GitHub API to report whether there is any new data, and falls
-- | back to the cache if there is not. Should only be used on GET requests; for
-- | POST requests, use `request`.
requestCached
  :: forall args
   . (Octokit -> Route -> Object String -> Record args -> ExceptT GitHubAPIError Aff Json)
  -> Octokit
  -> Ref GitHubCache
  -> Route
  -> Object String
  -> Record args
  -> Boolean
  -> ExceptT GitHubAPIError Aff Json
requestCached runRequest octokit cacheRef route@(Route routeStr) headers args checkGitHub = do
  cache <- liftEffect $ Ref.read cacheRef
  ExceptT $ case Map.lookup route cache of
    Nothing -> do
      log $ "NOT CACHED: No entry for " <> routeStr
      result <- Except.runExceptT $ runRequest octokit route headers args
      modified <- liftEffect getGitHubTime
      liftEffect $ Ref.modify_ (Map.insert route { modified, payload: result }) cacheRef
      pure result

    Just cached -> case cached.payload of
      Left err
        -- We don't retry 404 errors because they indicate a missing resource.
        | err.statusCode == 404 -> do
            pure $ Left err
        -- Otherwise, if we have an error in cache, we retry the request; we
        -- don't have anything usable we could return.
        | otherwise -> do
            log $ "CACHED ERROR: Deleting non-404 error entry and retrying " <> routeStr
            logShow err
            liftEffect $ Ref.modify_ (Map.delete route) cacheRef
            Except.runExceptT $ requestCached runRequest octokit cacheRef route headers args checkGitHub

      -- If we do have a usable cache value, then we will defer to GitHub's
      -- judgment on whether to use it or not.
      Right payload
        | checkGitHub -> do
            result <- Except.runExceptT $ runRequest octokit route (Object.insert "If-Modified-Since" cached.modified headers) args
            case result of
              -- 304 Not Modified means that we can rely on our local cache; nothing has
              -- changed for this resource.
              Left err | err.statusCode == 304 -> do
                pure $ Right payload
              _ -> do
                modified <- liftEffect getGitHubTime
                liftEffect $ Ref.modify_ (Map.insert route { modified, payload: result }) cacheRef
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
