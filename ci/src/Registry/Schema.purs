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

-- | PureScript encoding of ../v1/Manifest.dhall
newtype Manifest = Manifest
  { name :: PackageName
  , owners :: Maybe (NonEmptyArray Owner)
  , version :: Version
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
    "owners" := fields.owners
    "repository" := fields.repository
    "description" := fields.description
    "targets" := fields.targets

  decode json = Manifest <$> Json.decode json

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
  | Authenticated AuthenticatedData

derive instance eqOperation :: Eq Operation

derive instance genericOperation :: Generic.Generic Operation _

instance showOperation :: Show Operation where
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

data AuthenticatedOperation = Unpublish UnpublishData

derive instance Eq AuthenticatedOperation

instance RegistryJson AuthenticatedOperation where
  encode = case _ of
    Unpublish fields -> Json.encode fields

  decode json = do
    let parseUnpublish = Unpublish <$> Json.decode json
    parseUnpublish

instance Show AuthenticatedOperation where
  show = case _ of
    Unpublish inner -> "Unpublish (" <> show (showWithPackage inner) <> ")"
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
  , owners :: Maybe (NonEmptyArray Owner)
  , releases :: Object VersionMetadata
  , unpublished :: Object String
  }

type VersionMetadata =
  { ref :: String
  , hash :: Sha256
  , bytes :: Number
  , published :: RFC3339String
  }

mkNewMetadata :: Repo -> Metadata
mkNewMetadata location =
  { location
  , owners: Nothing
  , releases: Object.empty
  , unpublished: Object.empty
  }

addVersionToMetadata :: Version -> VersionMetadata -> Metadata -> Metadata
addVersionToMetadata version versionMeta metadata = do
  let releases = Object.insert (Version.printVersion version) versionMeta metadata.releases
  metadata { releases = releases }

unpublishVersionInMetadata :: Version -> String -> Metadata -> Metadata
unpublishVersionInMetadata version unpublishReason metadata = do
  let versionKey = Version.printVersion version
  let releases = Object.delete versionKey metadata.releases
  let unpublished = Object.insert versionKey unpublishReason metadata.unpublished
  metadata { releases = releases, unpublished = unpublished }

isVersionInMetadata :: Version -> Metadata -> Boolean
isVersionInMetadata version metadata = versionPublished || versionUnpublished
  where
  versionStr = Version.printVersion version
  versionPublished = isJust $ Object.lookup versionStr metadata.releases
  versionUnpublished = isJust $ Object.lookup versionStr metadata.unpublished
