module Foreign.GitHub where

import Registry.Prelude

import Control.Alt ((<|>))
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut ((.:))
import Data.Argonaut as Json
import Data.List as List
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
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

derive instance newtypeEvent :: Newtype Event _

instance eventDecodeJson :: Json.DecodeJson Event where
  decodeJson json = do
    obj <- Json.decodeJson json
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

parseRepo :: String -> Either Parser.ParseError Address
parseRepo = Parser.runParser do
  void $ Parse.string "https://github.com/"
    <|> Parse.string "git://github.com/"
    <|> Parse.string "git@github.com:"
  owner <- map (fromCharArray <<< List.toUnfoldable)
    $ ParseC.manyTill (ParseC.choice [ Parse.alphaNum, Parse.char '-' ]) (Parse.char '/')
  repoWithSuffix <- map (fromCharArray <<< List.toUnfoldable)
    $ ParseC.many Parse.anyChar
  let repo = fromMaybe repoWithSuffix $ String.stripSuffix (String.Pattern ".git") repoWithSuffix
  pure { owner, repo }

foreign import getReleasesImpl :: EffectFn2 Octokit Address (Promise (Array Tag))

getReleases :: Octokit -> Address -> Aff (Array Tag)
getReleases octokit address = Promise.toAffE $ runEffectFn2 getReleasesImpl octokit address

newtype IssueNumber = IssueNumber Int

instance newtypeIssueNumber :: Newtype IssueNumber Int
derive newtype instance eqIssueNumber :: Eq IssueNumber
derive newtype instance showIssueNumber :: Show IssueNumber

foreign import createCommentImpl :: EffectFn4 Octokit Address Int String (Promise Unit)

createComment :: Octokit -> IssueNumber -> String -> Aff Unit
createComment octokit issue body = Promise.toAffE $ runEffectFn4 createCommentImpl octokit registryAddress (coerce issue) body

foreign import closeIssueImpl :: EffectFn3 Octokit Address Int (Promise Unit)

closeIssue :: Octokit -> IssueNumber -> Aff Unit
closeIssue octokit issue = Promise.toAffE $ runEffectFn3 closeIssueImpl octokit registryAddress (coerce issue)
