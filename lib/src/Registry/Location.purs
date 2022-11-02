-- | Implementation of the `Location` data type from the registry spec.
-- | https://github.com/purescript/registry-dev/blob/master/SPEC.md#location
-- |
-- | There is also a Dhall spec for this data type:
-- | https://github.com/purescript/registry-dev/blob/master/specs/v1/Location.dhall
module Registry.Location
  ( GitData
  , GitHubData
  , Location(..)
  , codec
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CA.Record
import Data.Maybe (Maybe)
import Data.Profunctor as Profunctor
import Node.Path (FilePath)

-- | The location of a package; the registry uses this data type to decide how
-- | to fetch source code when publishing a new package version.
data Location
  = Git GitData
  | GitHub GitHubData

derive instance Eq Location

-- | A codec for encoding and decoding a `Location` as JSON. To see how each
-- | possible `Location` is represented, please see the relevant codec (for
-- | example the `githubCodec` or `gitCodec` implementations).
codec :: JsonCodec Location
codec = Codec.codec' decode encode
  where
  decode json =
    lmap (const (CA.TypeMismatch "Location")) do
      map Git (CA.decode gitCodec json)
      <|> map GitHub (CA.decode githubCodec json)

  encode = case _ of
    Git git -> CA.encode gitCodec git
    GitHub github -> CA.encode githubCodec github

-- | The location of a package within a GitHub repository
type GitHubData =
  { owner :: String
  , repo :: String
  , subdir :: Maybe FilePath
  }

-- | Encode `GitHubData` as a Json object. The JSON representation of the GitHub
-- | type uses 'githubOwner' and 'githubRepo', but in PureScript we use 'owner'
-- | and 'repo' for convenience.
githubCodec :: JsonCodec GitHubData
githubCodec = Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "GitHub"
  { githubOwner: CA.string
  , githubRepo: CA.string
  , subdir: CA.Record.optional CA.string
  }
  where
  toJsonRep { owner, repo, subdir } = { githubOwner: owner, githubRepo: repo, subdir }
  fromJsonRep { githubOwner, githubRepo, subdir } = { owner: githubOwner, repo: githubRepo, subdir }

-- | The location of a package within a Git repository
type GitData =
  { url :: String
  , subdir :: Maybe FilePath
  }

-- | Encode `GitData` as a Json object. The JSON representation of the GitHub
-- | type uses 'gitUrl' but in PureScript we use 'url' for convenience.
gitCodec :: JsonCodec GitData
gitCodec = Profunctor.dimap toJsonRep fromJsonRep $ CA.Record.object "Git"
  { gitUrl: CA.string
  , subdir: CA.Record.optional CA.string
  }
  where
  -- The JSON representation of the GitHub type uses 'gitUrl', but in PureScript
  -- we use 'url' for convenience.
  toJsonRep { url, subdir } = { gitUrl: url, subdir }
  fromJsonRep { gitUrl, subdir } = { url: gitUrl, subdir }
