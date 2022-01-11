module Registry.Scripts.LegacyImport.Error where

import Registry.Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as Common
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant (variantMatch)
import Data.Generic.Rep (class Generic)
import Data.Interpolate (i)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (dimap)
import Data.Variant as V
import Registry.Codec (neArray, newtypeStringCodec)
import Registry.PackageName (PackageName, packageNameCodec)
import Type.Proxy (Proxy(..))

-- | A map of error types to package names to package versions, where failed
-- | versions contain rich information about why they failed.
newtype PackageFailures = PackageFailures (Map ImportErrorKey (Map RawPackageName (Either ImportError (Map RawVersion ImportError))))

derive instance Newtype PackageFailures _

packageFailuresCodec :: JsonCodec PackageFailures
packageFailuresCodec = dimap unwrap wrap
  $ Common.map importErrorKeyCodec
  $ Common.map rawPackageNameCodec
  $ Common.either importErrorCodec
  $ Common.map rawVersionCodec importErrorCodec

-- | An import error printed as a key usable in a map
newtype ImportErrorKey = ImportErrorKey String

derive instance Newtype ImportErrorKey _
derive newtype instance Eq ImportErrorKey
derive newtype instance Ord ImportErrorKey
instance Show ImportErrorKey where
  show (ImportErrorKey key) = i "(ImportErrorKey " key ")"

importErrorKeyCodec :: JsonCodec ImportErrorKey
importErrorKeyCodec = newtypeStringCodec "ImportErrorKey"

-- | An unprocessed package name, which may possibly be malformed.
newtype RawPackageName = RawPackageName String

derive instance Newtype RawPackageName _
derive newtype instance Eq RawPackageName
derive newtype instance Ord RawPackageName

rawPackageNameCodec :: JsonCodec RawPackageName
rawPackageNameCodec = newtypeStringCodec "RawPackageName"

-- | An unprocessed version, taken from a GitHub tag
newtype RawVersion = RawVersion String

derive instance Newtype RawVersion _
derive newtype instance Eq RawVersion
derive newtype instance Ord RawVersion

rawVersionCodec :: JsonCodec RawVersion
rawVersionCodec = newtypeStringCodec "RawVersion"

-- | An error representing why a package version cannot be imported from the
-- | Bower registry.
data ImportError
  = InvalidGitHubRepo String
  | ResourceError ResourceError
  | MalformedPackageName String
  | NoDependencyFiles
  | NonRegistryDependencies (NonEmptyArray RawPackageName)
  | NoManifests
  | ManifestImportError (NonEmptyArray ManifestError)

derive instance Eq ImportError
derive instance Generic ImportError _

importErrorCodec :: JsonCodec ImportError
importErrorCodec = dimap toVariant fromVariant $ variantMatch
  { invalidGitHubRepo: Right CA.string
  , resourceError: Right resourceErrorCodec
  , malformedPackageName: Right CA.string
  , noDependencyFiles: Left unit
  , nonRegistryDependencies: Right (neArray rawPackageNameCodec)
  , noManifests: Left unit
  , manifestImportError: Right (neArray manifestErrorCodec)
  }
  where
  toVariant = case _ of
    InvalidGitHubRepo a -> V.inj (Proxy :: _ "invalidGitHubRepo") a
    ResourceError a ->  V.inj (Proxy :: _ "resourceError") a
    MalformedPackageName a ->  V.inj (Proxy :: _ "malformedPackageName") a
    NoDependencyFiles -> V.inj (Proxy :: _ "noDependencyFiles") unit
    NonRegistryDependencies a ->  V.inj (Proxy :: _ "nonRegistryDependencies") a
    NoManifests -> V.inj (Proxy :: _ "noManifests") unit
    ManifestImportError a ->  V.inj (Proxy :: _ "manifestImportError") a
  fromVariant = V.match
    { invalidGitHubRepo: InvalidGitHubRepo
    , resourceError: ResourceError
    , malformedPackageName: MalformedPackageName
    , noDependencyFiles: \_ -> NoDependencyFiles
    , nonRegistryDependencies: NonRegistryDependencies
    , noManifests: \_ -> NoManifests
    , manifestImportError: ManifestImportError
    }

manifestErrorKey :: ImportErrorKey
manifestErrorKey = ImportErrorKey "manifestError"

printImportErrorKey :: ImportError -> ImportErrorKey
printImportErrorKey = case _ of
  InvalidGitHubRepo _ -> ImportErrorKey "invalidGitHubRepo"
  ResourceError _ -> ImportErrorKey "resourceError"
  MalformedPackageName _ -> ImportErrorKey "malformedPackageName"
  NoDependencyFiles -> ImportErrorKey "noDependencyFiles"
  NonRegistryDependencies _ -> ImportErrorKey "nonRegistryDependencies"
  NoManifests -> ImportErrorKey "noManifests"
  ManifestImportError _ -> manifestErrorKey

-- | An error fetching a resource necessary to produce a Manifest for a
-- | given package.
type ResourceError = { resource :: RemoteResource, error :: RequestError }

resourceErrorCodec :: JsonCodec ResourceError
resourceErrorCodec = CAR.object "ResourceError"
  { resource: remoteResourceCodec
  , error: requestErrorCodec
  }

data RequestError = BadRequest | BadStatus Int | DecodeError String

derive instance Eq RequestError
derive instance Generic RequestError _

requestErrorCodec :: JsonCodec RequestError
requestErrorCodec = dimap toVariant fromVariant $ variantMatch
  { badRequest: Left unit
  , badStatus: Right CA.int
  , decodeError: Right CA.string
  }
  where
  toVariant = case _ of
    BadRequest -> V.inj (Proxy :: _ "badRequest") unit
    BadStatus a -> V.inj (Proxy :: _ "badStatus") a
    DecodeError a -> V.inj (Proxy :: _ "decodeError") a
  fromVariant = V.match
    { badRequest: \_ -> BadRequest
    , badStatus: BadStatus
    , decodeError: DecodeError
    }

-- | An error representing why a manifest could not be produced for this package
data ManifestError
  = MissingName
  | MissingLicense
  | BadLicense (Array String)
  | BadVersion String
  | InvalidDependencyNames (NonEmptyArray String)
  | BadDependencyVersions (NonEmptyArray { dependency :: PackageName, failedBounds :: String })

derive instance Eq ManifestError
derive instance Generic ManifestError _

manifestErrorCodec :: JsonCodec ManifestError
manifestErrorCodec = dimap toVariant fromVariant $ variantMatch
  { missingName: Left unit
  , missingLicense: Left unit
  , badLicense: Right (CA.array CA.string)
  , badVersion: Right CA.string
  , invalidDependencyNames: Right $ neArray CA.string
  , badDependencyVersions: Right $ neArray $ CAR.object "BadDepencencyVersionRecord"
      { dependency: packageNameCodec
      , failedBounds: CA.string
      }
  }
  where
  toVariant = case _ of
    MissingName -> V.inj (Proxy :: _ "missingName") unit
    MissingLicense -> V.inj (Proxy :: _ "missingLicense") unit
    BadLicense a -> V.inj (Proxy :: _ "badLicense") a
    BadVersion a -> V.inj (Proxy :: _ "badVersion") a
    InvalidDependencyNames a -> V.inj (Proxy :: _ "invalidDependencyNames") a
    BadDependencyVersions a -> V.inj (Proxy :: _ "badDependencyVersions") a
  fromVariant = V.match
    { missingName: \_ -> MissingName
    , missingLicense: \_ -> MissingLicense
    , badLicense: BadLicense
    , badVersion: BadVersion
    , invalidDependencyNames: InvalidDependencyNames
    , badDependencyVersions: BadDependencyVersions
    }


newtype ManifestErrorKey = ManifestErrorKey String

derive instance Newtype ManifestErrorKey _
derive newtype instance Eq ManifestErrorKey
derive newtype instance Ord ManifestErrorKey
instance Show ManifestErrorKey where
  show (ManifestErrorKey key) = i "(ManifestErrorKey " key ")"

printManifestErrorKey :: ManifestError -> ManifestErrorKey
printManifestErrorKey = ManifestErrorKey <<< case _ of
  MissingName -> "missingName"
  MissingLicense -> "missingLicense"
  BadLicense _ -> "badLicense"
  BadVersion _ -> "badVersion"
  InvalidDependencyNames _ -> "invalidDependencyNames"
  BadDependencyVersions _ -> "badDependencyVersions"

-- | A resource required for a package that has to be requested from a non-local
-- | location.
data RemoteResource = APIResource APIResource | FileResource FileResource

derive instance Eq RemoteResource
derive instance Generic RemoteResource _

remoteResourceCodec :: JsonCodec RemoteResource
remoteResourceCodec = dimap toVariant fromVariant $ variantMatch
  { apiResource: Right apiResourceCodec
  , fileResource: Right fileResourceCodec
  }
  where
  toVariant = case _ of
    APIResource a -> V.inj (Proxy :: _ "apiResource") a
    FileResource a -> V.inj (Proxy :: _ "fileResource") a

  fromVariant = V.match
    { apiResource: APIResource
    , fileResource: FileResource
    }


-- | A resource that has to be fetched via an API
data APIResource = GitHubReleases

derive instance Eq APIResource
derive instance Generic APIResource _

apiResourceCodec :: JsonCodec APIResource
apiResourceCodec = dimap toVariant fromVariant $ variantMatch
  { gitHubReleases: Left unit
  }
  where
  toVariant = case _ of
    GitHubReleases -> V.inj (Proxy :: _ "gitHubReleases") unit
  fromVariant = V.match
    { gitHubReleases: \_ -> GitHubReleases
    }

-- | A resource that has to be fetched via donwloading the relevant file
data FileResource
  = BowerJson
  | SpagoDhall
  | PackagesDhall
  | PackageJson
  | LicenseFile

derive instance Eq FileResource
derive instance Generic FileResource _

fileResourceCodec :: JsonCodec FileResource
fileResourceCodec = dimap toVariant fromVariant $ variantMatch
  { bowerJson: Left unit
  , spagoDhall: Left unit
  , packagesDhall: Left unit
  , packageJson: Left unit
  , licenseFile: Left unit
  }
  where
  toVariant = case _ of
    BowerJson -> V.inj (Proxy :: _ "bowerJson") unit
    SpagoDhall -> V.inj (Proxy :: _ "spagoDhall") unit
    PackagesDhall -> V.inj (Proxy :: _ "packagesDhall") unit
    PackageJson -> V.inj (Proxy :: _ "packageJson") unit
    LicenseFile -> V.inj (Proxy :: _ "licenseFile") unit
  fromVariant = V.match
    { bowerJson: \_ -> BowerJson
    , spagoDhall: \_ -> SpagoDhall
    , packagesDhall: \_ -> PackagesDhall
    , packageJson: \_ -> PackageJson
    , licenseFile: \_ -> LicenseFile
    }

fileResourcePath :: FileResource -> FilePath
fileResourcePath = case _ of
  BowerJson -> "bower.json"
  SpagoDhall -> "spago.dhall"
  PackagesDhall -> "packages.dhall"
  PackageJson -> "package.json"
  LicenseFile -> "LICENSE"
