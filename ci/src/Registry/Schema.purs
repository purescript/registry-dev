module Registry.Schema where

import Registry.Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as Common
import Data.Codec.Argonaut.Compat as Compat
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant (variantMatch)
import Data.Generic.Rep as Generic
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap)
import Data.Variant as V
import Foreign.Object as Object
import Foreign.SPDX (License, licenseCodec)
import Foreign.SemVer (Range, SemVer, rangeCodec, semVerCodec)
import Foreign.SemVer as SemVer
import Registry.PackageName (PackageName, packageNameCodec)
import Registry.PackageName as PackageName
import Type.Proxy (Proxy(..))

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

manifestCodec :: JsonCodec Manifest
manifestCodec = dimap unwrap wrap $ CAR.object "Manifest"
  { description: Compat.maybe CA.string
  , license: licenseCodec
  , name: packageNameCodec
  , repository: repoCodec
  , targets: Common.foreignObject targetCodec
  , version: semVerCodec
  }

type Target =
  { dependencies :: Object Range
  , sources :: Array String
  }

targetCodec :: JsonCodec Target
targetCodec = CAR.object "Target"
  { dependencies: Common.foreignObject rangeCodec
  , sources: CA.array CA.string
  }

type RepoData d =
  { subdir :: Maybe String
  | d
  }

type GitHubData = RepoData
  ( owner :: String
  , repo :: String
  )

gitHubDataCodec :: JsonCodec GitHubData
gitHubDataCodec = CAR.object "GitHubData"
  { repo: CA.string
  , owner: CA.string
  , subdir: Compat.maybe CA.string
  }

type GitData = RepoData (url :: String)

gitDataCodec :: JsonCodec GitData
gitDataCodec = CAR.object "GitData"
  { url: CA.string
  , subdir: Compat.maybe CA.string
  }

data Repo
  = Git GitData
  | GitHub GitHubData

derive instance eqRepo :: Eq Repo

derive instance genericRepo :: Generic.Generic Repo _

instance showRepo :: Show Repo where
  show = genericShow

-- | We encode it this way so that json-to-dhall can read it
repoCodec :: JsonCodec Repo
repoCodec = dimap toVariant fromVariant $ variantMatch
  { git: Right gitDataCodec
  , gitHub: Right gitHubDataCodec
  }
  where
  toVariant = case _ of
    Git a -> V.inj (Proxy :: _ "git") a
    GitHub a -> V.inj (Proxy :: _ "gitHub") a
  fromVariant = V.match
    { git: Git
    , gitHub: GitHub
    }

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

operationCodec :: JsonCodec Operation
operationCodec = dimap toVariant fromVariant $ variantMatch
  { addition: Right additionDataCodec
  , update: Right updateDataCodec
  , unpublish: Right unpublishDataCodec
  }
  where
  toVariant = case _ of
    Addition a -> V.inj (Proxy :: _ "addition") a
    Update a -> V.inj (Proxy :: _ "update") a
    Unpublish a -> V.inj (Proxy :: _ "unpublish") a
  fromVariant = V.match
    { addition: Addition
    , update: Update
    , unpublish: Unpublish
    }

type AdditionData =
  { addToPackageSet :: Boolean
  , fromBower :: Boolean
  , newPackageLocation :: Repo
  , newRef :: String
  , packageName :: PackageName
  }

additionDataCodec :: JsonCodec AdditionData
additionDataCodec = CAR.object "AdditionData"
    { packageName: packageNameCodec
    , addToPackageSet: CA.boolean
    , fromBower: CA.boolean
    , newPackageLocation: repoCodec
    , newRef: CA.string
    }

type UpdateData =
  { packageName :: PackageName
  , fromBower :: Boolean
  , updateRef :: String
  }

updateDataCodec :: JsonCodec UpdateData
updateDataCodec = CAR.object "UpdateData"
    { packageName: packageNameCodec
    , fromBower: CA.boolean
    , updateRef: CA.string
    }

type UnpublishData =
  { packageName :: PackageName
  , unpublishVersion :: SemVer
  , unpublishReason :: String
  }

unpublishDataCodec :: JsonCodec UnpublishData
unpublishDataCodec = CAR.object "UnpublishData"
    { packageName: packageNameCodec
    , unpublishVersion: semVerCodec
    , unpublishReason: CA.string
    }

type Metadata =
  { location :: Repo
  , releases :: Object VersionMetadata
  , unpublished :: Object String
  }

metadataCodec :: JsonCodec Metadata
metadataCodec = CAR.object "Metadata"
  { location: repoCodec
  , releases: Common.foreignObject versionMetadataCodec
  , unpublished: Common.foreignObject CA.string
  }

type VersionMetadata =
  { ref :: String
  , hash :: String
  }

versionMetadataCodec :: JsonCodec VersionMetadata
versionMetadataCodec = CAR.object "VersionMetadata"
  { ref: CA.string
  , hash: CA.string
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
