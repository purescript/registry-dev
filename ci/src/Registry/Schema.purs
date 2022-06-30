module Registry.Schema where

import Registry.Prelude

import Data.Date (Date)
import Data.Generic.Rep as Generic
import Data.Map as Map
import Data.RFC3339String (RFC3339String)
import Foreign.SPDX (License)
import Registry.Hash (Sha256)
import Registry.Json ((.:), (.:?), (:=))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
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
derive newtype instance Show Owner
derive newtype instance RegistryJson Owner

newtype PackageSet = PackageSet
  { compiler :: Version
  , publishedTime :: RFC3339String
  , packages :: Map PackageName Version
  , version :: Version
  }

derive instance Newtype PackageSet _
derive newtype instance Eq PackageSet
derive newtype instance Show PackageSet

instance RegistryJson PackageSet where
  encode (PackageSet plan) = Json.encode plan
  decode = map PackageSet <<< Json.decode

newtype LegacyPackageSet = LegacyPackageSet (Map PackageName LegacyPackageSetEntry)

derive instance Newtype LegacyPackageSet _
derive newtype instance Eq LegacyPackageSet
derive newtype instance Show LegacyPackageSet

instance RegistryJson LegacyPackageSet where
  encode (LegacyPackageSet plan) = Json.encode plan
  decode = map LegacyPackageSet <<< Json.decode

newtype LegacyPackageSetEntry = LegacyPackageSetEntry
  { dependencies :: Array PackageName
  , repo :: String
  , version :: RawVersion
  }

derive instance Newtype LegacyPackageSetEntry _
derive newtype instance Eq LegacyPackageSetEntry
derive newtype instance Show LegacyPackageSetEntry

instance RegistryJson LegacyPackageSetEntry where
  encode (LegacyPackageSetEntry plan) = Json.encode plan
  decode = map LegacyPackageSetEntry <<< Json.decode

-- | A compiler version and exact dependency versions that should be used to
-- | compile a newly-uploaded package as an API verification check.
-- |
-- | The build plan verification is NOT used for legacy packages.
newtype BuildPlan = BuildPlan
  { compiler :: Version
  , resolutions :: Map PackageName Version
  }

derive instance Newtype BuildPlan _
derive newtype instance Eq BuildPlan
derive newtype instance Show BuildPlan

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

derive instance Generic.Generic Location _

instance Show Location where
  show = genericShow

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

-- | PureScript encoding of ../v1/Operation.dhall
data Operation
  = Addition AdditionData
  | Update UpdateData
  | Authenticated AuthenticatedData

derive instance Eq Operation

derive instance Generic.Generic Operation _

instance Show Operation where
  show = case _ of
    Addition inner -> "Addition (" <> show (showWithPackage inner) <> ")"
    Update inner -> "Update (" <> show (showWithPackage inner) <> ")"
    Authenticated inner -> "Authenticated (" <> show inner <> ")"
    where
    showWithPackage :: forall r. { packageName :: PackageName | r } -> { packageName :: String | r }
    showWithPackage inner =
      inner { packageName = "PackageName (" <> PackageName.print inner.packageName <> ")" }

instance RegistryJson Operation where
  encode = case _ of
    Addition fields -> Json.encode fields
    Update fields -> Json.encode fields
    Authenticated fields -> Json.encode fields

  decode json = do
    let parseAddition = Addition <$> Json.decode json
    let parseUpdate = Update <$> Json.decode json
    let parseAuthenticated = Authenticated <$> Json.decode json
    parseAddition <|> parseUpdate <|> parseAuthenticated

data AuthenticatedOperation
  = Unpublish UnpublishData
  | Transfer TransferData

derive instance Eq AuthenticatedOperation

instance RegistryJson AuthenticatedOperation where
  encode = case _ of
    Unpublish fields -> Json.encode fields
    Transfer fields -> Json.encode fields

  decode json = do
    let parseUnpublish = Unpublish <$> Json.decode json
    let parseTransfer = Transfer <$> Json.decode json
    parseUnpublish <|> parseTransfer

instance Show AuthenticatedOperation where
  show = case _ of
    Unpublish inner -> "Unpublish (" <> show (showWithPackage inner) <> ")"
    Transfer inner -> "Transfer (" <> show (showWithPackage inner) <> ")"
    where
    showWithPackage :: forall r. { packageName :: PackageName | r } -> { packageName :: String | r }
    showWithPackage inner =
      inner { packageName = "PackageName (" <> PackageName.print inner.packageName <> ")" }

newtype AuthenticatedData = AuthenticatedData
  { payload :: AuthenticatedOperation
  -- We include the unparsed payload for use in verification so as to preserve
  -- any quirks of formatting that could change the input.
  , rawPayload :: String
  , signature :: Array String
  , email :: String
  }

derive instance Newtype AuthenticatedData _
derive newtype instance Eq AuthenticatedData
derive newtype instance Show AuthenticatedData

instance RegistryJson AuthenticatedData where
  encode (AuthenticatedData fields) = Json.encode fields
  decode json = do
    obj <- Json.decode json
    rawPayload <- obj .: "payload"
    payload <- Json.parseJson rawPayload
    signature <- obj .: "signature"
    email <- obj .: "email"
    pure $ AuthenticatedData { rawPayload, payload, signature, email }

type AdditionData =
  { newPackageLocation :: Location
  , newRef :: String
  , packageName :: PackageName
  , buildPlan :: BuildPlan
  }

type UpdateData =
  { packageName :: PackageName
  , updateRef :: String
  , buildPlan :: BuildPlan
  }

type UnpublishData =
  { packageName :: PackageName
  , unpublishVersion :: Version
  , unpublishReason :: String
  }

type TransferData =
  { packageName :: PackageName
  , newPackageLocation :: Location
  }

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
