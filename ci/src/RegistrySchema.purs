module Registry.Schema where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (jsonEmptyObject, (~>), (~>?), (:=), (:=?), (.:), (.:?), (.!=))
import Data.Argonaut as Json
import Data.Maybe (Maybe)
import Foreign.Object as Foreign


-- | PureScript encoding of ../v1/Manifest.dhall
type Manifest =
  { name :: String
  , version :: String -- TODO: we should have a newtype for this
  , license :: String
  , repository :: Repo
  , targets :: Foreign.Object Target
  }

type Target =
  { dependencies :: Foreign.Object String
  , sources :: Array String
  }


type RepoData d =
  { ref :: String
  , subdir :: Maybe String
  | d
  }

type GitHubData = RepoData
  ( owner :: String
  , repo :: String
  )

type GitData = RepoData ( url :: String )

data Repo
  = Git GitData
  | GitHub GitHubData

-- | We encode it this way so that json-to-dhall can read it
instance repoEncodeJson :: Json.EncodeJson Repo where
  encodeJson = case _ of
    Git { subdir, url, ref }
      -> "url" := url
      ~> "ref" := ref
      ~> "subdir" :=? subdir
      ~>? jsonEmptyObject
    GitHub { repo, owner, ref, subdir }
      -> "githubRepo" := repo
      ~> "githubOwner" := owner
      ~> "ref" := ref
      ~> "subdir" :=? subdir
      ~>? jsonEmptyObject

instance repoDecodeJson :: Json.DecodeJson Repo where
  decodeJson json = do
    obj <- Json.decodeJson json
    ref <- obj .: "ref"
    subdir <- obj .:? "subdir" .!= mempty
    let parseGitHub = do
          owner <- obj .: "githubOwner"
          repo <- obj .: "githubRepo"
          pure $ GitHub { owner, repo, ref, subdir }
    let parseGit = do
          url <- obj .: "url"
          pure $ Git { url, ref, subdir }
    parseGitHub <|> parseGit
