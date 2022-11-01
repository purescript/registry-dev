module Registry.Schema where

import Registry.Prelude

import Data.DateTime (DateTime)
import Data.Formatter.DateTime (Formatter, FormatterCommand(..))
import Data.Formatter.DateTime as Formatter.DateTime
import Data.List as List
import Data.Map as Map
import Data.RFC3339String (RFC3339String)
import Registry.Json ((.:), (.:?), (:=))
import Registry.Json as Json
import Registry.License (License)
import Registry.PackageName (PackageName)
import Registry.Sha256 (Sha256)
import Registry.Version (Range, Version)

-- | PureScript encoding of ../v1/Manifest.dhall
newtype Manifest = Manifest
  { name :: PackageName
  , owners :: Maybe (NonEmptyArray Owner)
  , version :: Version
  , license :: License
  , location :: Location
  , description :: Maybe String
  , files :: Maybe (Array String)
  , dependencies :: Map PackageName Range
  }

derive instance Eq Manifest
derive instance Newtype Manifest _

instance RegistryJson Manifest where
  encode (Manifest fields) = Json.encodeObject do
    "name" := fields.name
    "version" := fields.version
    "license" := fields.license
    "location" := fields.location
    "owners" := fields.owners
    "description" := fields.description
    "files" := fields.files
    "dependencies" := fields.dependencies

  decode json = do
    manifestFields <- Json.decode json
    pure $ Manifest manifestFields

-- | A package owner, described using their SSH key and associated email address. It
-- | is not necessary to provide a valid email address, but the email address
-- | provided must match the one used to sign payloads.
-- |
-- | https://man.openbsd.org/ssh-keygen#ALLOWED_SIGNERS
newtype Owner = Owner
  { email :: String
  , keytype :: String
  , public :: String
  }

derive instance Newtype Owner _
derive newtype instance Eq Owner
derive newtype instance RegistryJson Owner

dateFormatter :: Formatter
dateFormatter = List.fromFoldable
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  ]

newtype PackageSet = PackageSet
  { compiler :: Version
  , published :: DateTime
  , packages :: Map PackageName Version
  , version :: Version
  }

derive instance Newtype PackageSet _
derive newtype instance Eq PackageSet

instance RegistryJson PackageSet where
  -- This instance is manually encoded so we can control the order of fields.
  encode (PackageSet set) = Json.encodeObject do
    "version" := set.version
    "compiler" := set.compiler
    "published" := Formatter.DateTime.format dateFormatter set.published
    "packages" := set.packages
  decode json = do
    fields <- Json.decode json
    published <- Formatter.DateTime.unformat dateFormatter fields.published
    pure $ PackageSet $ fields { published = published }

-- | A compiler version and exact dependency versions that should be used to
-- | compile a newly-uploaded package as an API verification check.
newtype BuildPlan = BuildPlan
  { compiler :: Version
  , resolutions :: Maybe (Map PackageName Version)
  }

derive instance Newtype BuildPlan _
derive newtype instance Eq BuildPlan

instance RegistryJson BuildPlan where
  encode (BuildPlan plan) = Json.encode plan
  decode = map BuildPlan <<< Json.decode

type LocationData d =
  { subdir :: Maybe String
  | d
  }

type GitHubData = LocationData
  ( owner :: String
  , repo :: String
  )

type GitData = LocationData (gitUrl :: String)

data Location
  = Git GitData
  | GitHub GitHubData

derive instance Eq Location

-- | We encode it this way so that json-to-dhall can read it
instance RegistryJson Location where
  encode = Json.encodeObject <<< case _ of
    Git { subdir, gitUrl } -> do
      "gitUrl" := gitUrl
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
        gitUrl <- obj .: "gitUrl"
        pure $ Git { gitUrl, subdir }
    parseGitHub <|> parseGit

type Metadata =
  { location :: Location
  , owners :: Maybe (NonEmptyArray Owner)
  , published :: Map Version PublishedMetadata
  , unpublished :: Map Version UnpublishedMetadata
  }

type PublishedMetadata =
  { ref :: String
  , hash :: Sha256
  , bytes :: Number
  , publishedTime :: RFC3339String
  }

type UnpublishedMetadata =
  { ref :: String
  , reason :: String
  , publishedTime :: RFC3339String
  , unpublishedTime :: RFC3339String
  }

mkNewMetadata :: Location -> Metadata
mkNewMetadata location =
  { location
  , owners: Nothing
  , published: Map.empty
  , unpublished: Map.empty
  }

addVersionToMetadata :: Version -> PublishedMetadata -> Metadata -> Metadata
addVersionToMetadata version versionMeta metadata = do
  let published = Map.insert version versionMeta metadata.published
  metadata { published = published }

unpublishVersionInMetadata :: Version -> UnpublishedMetadata -> Metadata -> Metadata
unpublishVersionInMetadata version versionMeta metadata = do
  let published = Map.delete version metadata.published
  let unpublished = Map.insert version versionMeta metadata.unpublished
  metadata { published = published, unpublished = unpublished }

isVersionInMetadata :: Version -> Metadata -> Boolean
isVersionInMetadata version metadata = versionPublished || versionUnpublished
  where
  versionPublished = isJust $ Map.lookup version metadata.published
  versionUnpublished = isJust $ Map.lookup version metadata.unpublished
