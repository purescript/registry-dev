module GitHub where

import Registry.Prelude

import Control.Alt ((<|>))
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Argonaut ((.:))
import Data.Argonaut as Json
import Data.List as List
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Safe.Coerce (coerce)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodePoints as Parse
import Text.Parsing.StringParser.Combinators as ParseC

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
    $ ParseC.manyTill (ParseC.choice [Parse.alphaNum, Parse.char '-']) (Parse.char '/')
  repoWithSuffix <- map (fromCharArray <<< List.toUnfoldable)
    $ ParseC.many Parse.anyChar
  let repo = fromMaybe repoWithSuffix $ String.stripSuffix (String.Pattern ".git") repoWithSuffix
  pure { owner, repo }

foreign import getReleasesImpl :: EffectFn1 Address (Promise (Array Tag))

getReleases :: Address -> Aff (Array Tag)
getReleases = Promise.toAffE <<< runEffectFn1 getReleasesImpl

newtype CommentId = CommentId String
instance newtypeCommentId :: Newtype CommentId String
derive newtype instance showCommentId :: Show CommentId

newtype IssueNumber = IssueNumber Int
instance newtypeIssueNumber :: Newtype IssueNumber Int
derive newtype instance eqIssueNumber :: Eq IssueNumber
derive newtype instance showIssueNumber :: Show IssueNumber

foreign import createCommentImpl :: EffectFn3 Address Int String (Promise String)

createComment :: IssueNumber -> String -> Aff CommentId
createComment issue body =
  coerce
    $ Promise.toAffE
    $ runEffectFn3 createCommentImpl registryAddress (coerce issue) body
