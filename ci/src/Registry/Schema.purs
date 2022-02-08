module Registry.Schema where

import Registry.Prelude

import Data.Generic.Rep as Generic
import Data.RFC3339String (RFC3339String)
import Foreign.Object as Object
import Foreign.SPDX (License)
import Registry.Hash (Sha256)
import Registry.Json ((.:), (.:?), (:=))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version (Range, Version)
import Registry.Version as Version
import Text.Parsing.StringParser as StringParser

-- | PureScript encoding of ../v1/Manifest.dhall
newtype Manifest = Manifest
  { name :: PackageName
  , version :: Version
  , license :: License
  , description :: Maybe String
  , repository :: Repo
  , dependencies :: Map PackageName Range
  }

derive instance Eq Manifest
derive instance Newtype Manifest _

instance RegistryJson Manifest where
  encode (Manifest fields) = Json.encodeObject do
    "name" := fields.name
    "version" := fields.version
    "license" := fields.license
    "description" := fields.description
    "repository" := fields.repository
    "dependencies" := mapKeys PackageName.print fields.dependencies

  decode json = do
    manifestFields <- Json.decode json
    let parse = lmap StringParser.printParserError <<< PackageName.parse
    parsed <- traverseKeys parse manifestFields.dependencies
    pure $ Manifest $ manifestFields { dependencies = parsed }

type RepoData d =
  { subdir :: Maybe String
  | d
  }

type GitHubData = RepoData
  ( owner :: String
  , repo :: String
  )

type GitData = RepoData (url :: String)

data Repo
  = Git GitData
  | GitHub GitHubData

derive instance eqRepo :: Eq Repo

derive instance genericRepo :: Generic.Generic Repo _

instance showRepo :: Show Repo where
  show = genericShow

-- | We encode it this way so that json-to-dhall can read it
instance RegistryJson Repo where
  encode = Json.encodeObject <<< case _ of
    Git { subdir, url } -> do
      "url" := url
      "subdir" := subdir
    GitHub { repo, owner, subdir } -> do
      "githubOwner" := owner
      "githubRepo" := repo
      "subdir" := subdir

  decode json = do
    obj <- Json.decode json
    subdir <- fromMaybe mempty <$> obj .:? "subdir"
    let
      parseGitHub = do
        owner <- obj .: "githubOwner"
        repo <- obj .: "githubRepo"
        pure $ GitHub { owner, repo, subdir }
    let
      parseGit = do
        url <- obj .: "url"
        pure $ Git { url, subdir }
    parseGitHub <|> parseGit

-- | PureScript encoding of ../v1/Operation.dhall
data Operation
  = Addition AdditionData
  | Update UpdateData
  | Unpublish UnpublishData

derive instance eqOperation :: Eq Operation

derive instance genericOperation :: Generic.Generic Operation _

instance showOperation :: Show Operation where
  show = case _ of
    Addition inner -> "Addition (" <> show (showWithPackage inner) <> ")"
    Update inner -> "Update (" <> show (showWithPackage inner) <> ")"
    Unpublish inner -> "Unpublish (" <> show (showWithPackage inner) <> ")"
    where
    showWithPackage :: forall r. { packageName :: PackageName | r } -> { packageName :: String | r }
    showWithPackage inner =
      inner { packageName = "PackageName (" <> PackageName.print inner.packageName <> ")" }

instance RegistryJson Operation where
  encode = case _ of
    Addition fields -> Json.encode fields
    Update fields -> Json.encode fields
    Unpublish fields -> Json.encode fields

  decode json = do
    let parseAddition = Addition <$> Json.decode json
    let parseUpdate = Update <$> Json.decode json
    let parseUnpublish = Unpublish <$> Json.decode json
    parseAddition <|> parseUpdate <|> parseUnpublish

type AdditionData =
  { newPackageLocation :: Repo
  , newRef :: String
  , packageName :: PackageName
  }

type UpdateData =
  { packageName :: PackageName
  , updateRef :: String
  }

type UnpublishData =
  { packageName :: PackageName
  , unpublishVersion :: Version
  , unpublishReason :: String
  }

type Metadata =
  { location :: Repo
  , releases :: Object VersionMetadata
  , unpublished :: Object String
  }

type VersionMetadata =
  { ref :: String
  , hash :: Sha256
  , published :: RFC3339String
  , bytes :: Number
  }

mkNewMetadata :: Repo -> Metadata
mkNewMetadata location =
  { location
  , releases: Object.empty
  , unpublished: Object.empty
  }

addVersionToMetadata :: Version -> VersionMetadata -> Metadata -> Metadata
addVersionToMetadata version versionMeta metadata = do
  let releases = Object.insert (Version.printVersion version) versionMeta metadata.releases
  metadata { releases = releases }

isVersionInMetadata :: Version -> Metadata -> Boolean
isVersionInMetadata version metadata = versionPublished || versionUnpublished
  where
  versionStr = Version.printVersion version
  versionPublished = isJust $ Object.lookup versionStr metadata.releases
  versionUnpublished = isJust $ Object.lookup versionStr metadata.unpublished
