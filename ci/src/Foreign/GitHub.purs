module Foreign.GitHub where

import Registry.Prelude

import Affjax.StatusCode as Http
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Bitraversable (ltraverse)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.List as List
import Data.Newtype (unwrap)
import Data.Number as Number
import Data.RFC3339String (RFC3339String(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Time.Duration (Milliseconds(..))
import Effect.Exception as Exception
import Effect.Uncurried (EffectFn4, EffectFn5, EffectFn6, runEffectFn4, runEffectFn5, runEffectFn6)
import Registry.Json ((.:))
import Registry.Json as Json
import Safe.Coerce (coerce)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodePoints as Parse
import Text.Parsing.StringParser.Combinators as ParseC

foreign import data Octokit :: Type

foreign import mkOctokit :: Effect Octokit

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
    , ratelimitReset: unsafeFromJust $ Instant.instant $ Milliseconds $ ratelimitReset * 1000.0
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
