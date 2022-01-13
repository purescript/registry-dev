module Registry.Schema where

import Registry.Prelude

import Data.Generic.Rep as Generic
import Foreign.Object as Object
import Foreign.SPDX (License)
import Foreign.SemVer (SemVer, Range)
import Foreign.SemVer as SemVer
import Registry.Json ((.:), (.:?), (:=))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName

-- | PureScript encoding of ../v1/Manifest.dhall
newtype Manifest = Manifest
  { name :: PackageName
  , version :: SemVer
  , license :: License
  , repository :: Repo
  , targets :: Object Target
  , description :: Maybe String
  }

derive instance Eq Manifest
derive instance Newtype Manifest _

instance RegistryJson Manifest where
  encode (Manifest fields) = Json.encodeObject do
    "name" := fields.name
    "version" := fields.version
    "license" := fields.license
    "repository" := fields.repository
    "description" := fields.description
    "targets" := fields.targets

  decode json = Manifest <$> Json.decode json

newtype Target = Target
  { dependencies :: Object Range
  , sources :: Array String
  }

derive instance Newtype Target _
derive newtype instance Eq Target
derive newtype instance Show Target

-- We write a manual instance here to control the ordering of the resulting
-- object, which we don't want to be alphabetical
instance RegistryJson Target where
  encode (Target fields) = Json.encodeObject do
    "sources" := fields.sources
    "dependencies" := fields.dependencies
  decode json = Target <$> Json.decode json

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
  { addToPackageSet :: Boolean
  , fromBower :: Boolean
  , newPackageLocation :: Repo
  , newRef :: String
  , packageName :: PackageName
  }

type UpdateData =
  { packageName :: PackageName
  , fromBower :: Boolean
  , updateRef :: String
  }

type UnpublishData =
  { packageName :: PackageName
  , unpublishVersion :: SemVer
  , unpublishReason :: String
  }

type Metadata =
  { location :: Repo
  , releases :: Object VersionMetadata
  , unpublished :: Object String
  }

type VersionMetadata =
  { ref :: String
  , hash :: String
  }

mkNewMetadata :: Repo -> Metadata
mkNewMetadata location = { location, releases: mempty, unpublished: mempty }

addVersionToMetadata :: SemVer -> VersionMetadata -> Metadata -> Metadata
addVersionToMetadata version versionMeta metadata =
  metadata { releases = Object.insert (SemVer.version version) versionMeta metadata.releases }

isVersionInMetadata :: SemVer -> Metadata -> Boolean
isVersionInMetadata version metadata = versionPublished || versionUnpublished
  where
  versionStr = SemVer.version version
  versionPublished = isJust $ Object.lookup versionStr metadata.releases
  versionUnpublished = isJust $ Object.lookup versionStr metadata.unpublished
