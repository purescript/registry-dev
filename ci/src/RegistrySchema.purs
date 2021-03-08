module Registry.Schema where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (jsonEmptyObject, (~>), (~>?), (:=), (:=?), (.:), (.:?), (.!=))
import Data.Argonaut as Json
import Data.Generic.Rep as Generic
import Data.Show.Generic (genericShow)
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
  { subdir :: Maybe String
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

derive instance genericRepo :: Generic.Generic Repo _

instance showRepo :: Show Repo where
  show = genericShow

-- | We encode it this way so that json-to-dhall can read it
instance repoEncodeJson :: Json.EncodeJson Repo where
  encodeJson = case _ of
    Git { subdir, url }
      -> "url" := url
      ~> "subdir" :=? subdir
      ~>? jsonEmptyObject
    GitHub { repo, owner, subdir }
      -> "githubRepo" := repo
      ~> "githubOwner" := owner
      ~> "subdir" :=? subdir
      ~>? jsonEmptyObject

instance repoDecodeJson :: Json.DecodeJson Repo where
  decodeJson json = do
    obj <- Json.decodeJson json
    subdir <- obj .:? "subdir" .!= mempty
    let parseGitHub = do
          owner <- obj .: "githubOwner"
          repo <- obj .: "githubRepo"
          pure $ GitHub { owner, repo, subdir }
    let parseGit = do
          url <- obj .: "url"
          pure $ Git { url, subdir }
    parseGitHub <|> parseGit

-- | PureScript encoding of ../v1/Operation.dhall
data Operation
  = Addition AdditionData
  | Update UpdateData
  | Unpublish UnpublishData

derive instance genericOperation :: Generic.Generic Operation _

instance showOperation :: Show Operation where
  show = genericShow

instance operationDecodeJson :: Json.DecodeJson Operation where
  decodeJson json = do
    o <- Json.decodeJson json
    packageName <- o .: "packageName"
    let parseAddition = do
          addToPackageSet <- o .: "addToPackageSet"
          fromBower <- o .: "fromBower"
          newPackageLocation <- o .: "newPackageLocation"
          newRef <- o .: "newRef"
          pure $ Addition { newRef, packageName, addToPackageSet, fromBower, newPackageLocation }
    let parseUpdate = do
          fromBower <- o .: "fromBower"
          updateRef <- o .: "updateRef"
          pure $ Update { packageName, fromBower, updateRef }
    let parseUnpublish = do
          unpublishVersion <- o .: "unpublishVersion"
          unpublishReason <- o .: "unpublishReason"
          pure $ Unpublish { packageName, unpublishVersion, unpublishReason }
    parseAddition <|> parseUpdate <|> parseUnpublish

type AdditionData =
  { addToPackageSet :: Boolean
  , fromBower :: Boolean
  , newPackageLocation :: Repo
  , newRef :: String
  , packageName :: String
}

type UpdateData =
  { packageName :: String
  , fromBower :: Boolean
  , updateRef :: String
  }

type UnpublishData =
  { packageName :: String
  , unpublishVersion :: String
  , unpublishReason :: String
  }

type Metadata =
  { location :: Repo
  , releases :: Foreign.Object (Array Revision)
  , unpublished :: Foreign.Object String
  , maintainers :: Array String
  }

type Revision =
  { ref :: String
  , hash :: String
  }