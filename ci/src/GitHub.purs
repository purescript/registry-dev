module GitHub where

import Prelude

import Control.Alt ((<|>))
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Aff (Aff)
import Text.Parsing.StringParser as Parser
import Text.Parsing.StringParser.CodePoints as Parse
import Text.Parsing.StringParser.Combinators as ParseC

type Address = { owner :: String, repo :: String }

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

foreign import getReleasesImpl :: Fn2 String String (Effect (Promise (Array Tag)))
getReleases :: Address -> Aff (Array Tag)
getReleases { owner, repo } = Promise.toAffE (runFn2 getReleasesImpl owner repo)