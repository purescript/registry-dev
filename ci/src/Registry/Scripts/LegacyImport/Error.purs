module Registry.Scripts.LegacyImport.Error where

import Registry.Prelude

import Data.Argonaut as Json
import Data.Argonaut.Decode.Generic as Json.Decode.Generic
import Data.Argonaut.Encode.Generic as Json.Encode.Generic
import Data.Argonaut.Types.Generic as Json.Generic
import Data.Generic.Rep (class Generic)
import Data.Interpolate (i)
import Registry.PackageName (PackageName)
import Safe.Coerce (coerce)

-- | A map of error types to package names to package versions, where failed
-- | versions contain rich information about why they failed.
newtype PackageFailures = PackageFailures (Map ImportErrorKey (Map RawPackageName (Either ImportError (Map RawVersion ImportError))))

derive instance Newtype PackageFailures _

instance Json.EncodeJson PackageFailures where
  encodeJson failures =
    Json.encodeJson
      $ objectFromMap coerce
      $ map (objectFromMap coerce)
      $ map (map (map (objectFromMap coerce)))
      $ un PackageFailures failures

instance Json.DecodeJson PackageFailures where
  decodeJson json = do
    failuresObject :: Object (Object (Either ImportError (Object ImportError))) <- Json.decodeJson json
    pure
      $ PackageFailures
      $ objectToMap (Just <<< ImportErrorKey)
      $ map (objectToMap (Just <<< RawPackageName))
      $ map (map (map (objectToMap (Just <<< RawVersion)))) failuresObject

-- | An import error printed as a key usable in a map
newtype ImportErrorKey = ImportErrorKey String

derive instance Newtype ImportErrorKey _
derive newtype instance Eq ImportErrorKey
derive newtype instance Ord ImportErrorKey
instance Show ImportErrorKey where
  show (ImportErrorKey key) = i "(ImportErrorKey " key ")"

-- | An unprocessed package name, which may possibly be malformed.
newtype RawPackageName = RawPackageName String

derive instance Newtype RawPackageName _
derive newtype instance Eq RawPackageName
derive newtype instance Ord RawPackageName
derive newtype instance Json.EncodeJson RawPackageName
derive newtype instance Json.DecodeJson RawPackageName

-- | An unprocessed version, taken from a GitHub tag
newtype RawVersion = RawVersion String

derive instance Newtype RawVersion _
derive newtype instance Eq RawVersion
derive newtype instance Ord RawVersion
derive newtype instance Json.EncodeJson RawVersion
derive newtype instance Json.DecodeJson RawVersion

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

instance Json.EncodeJson ImportError where
  encodeJson = Json.Encode.Generic.genericEncodeJsonWith encodingOptions

instance Json.DecodeJson ImportError where
  decodeJson = Json.Decode.Generic.genericDecodeJsonWith encodingOptions

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

data RequestError = BadRequest | BadStatus Int | DecodeError String

derive instance Eq RequestError
derive instance Generic RequestError _

instance Json.EncodeJson RequestError where
  encodeJson = Json.Encode.Generic.genericEncodeJsonWith encodingOptions

instance Json.DecodeJson RequestError where
  decodeJson = Json.Decode.Generic.genericDecodeJsonWith encodingOptions

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

instance Json.EncodeJson ManifestError where
  encodeJson = Json.Encode.Generic.genericEncodeJsonWith encodingOptions

instance Json.DecodeJson ManifestError where
  decodeJson = Json.Decode.Generic.genericDecodeJsonWith encodingOptions

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

encodingOptions :: Json.Generic.Encoding
encodingOptions = Json.Generic.defaultEncoding { unwrapSingleArguments = true }

-- | A resource required for a package that has to be requested from a non-local
-- | location.
data RemoteResource = APIResource APIResource | FileResource FileResource

derive instance Eq RemoteResource
derive instance Generic RemoteResource _

instance Json.EncodeJson RemoteResource where
  encodeJson = Json.Encode.Generic.genericEncodeJsonWith encodingOptions

instance Json.DecodeJson RemoteResource where
  decodeJson = Json.Decode.Generic.genericDecodeJsonWith encodingOptions

-- | A resource that has to be fetched via an API
data APIResource = GitHubReleases

derive instance Eq APIResource
derive instance Generic APIResource _

instance Json.EncodeJson APIResource where
  encodeJson = Json.Encode.Generic.genericEncodeJsonWith encodingOptions

instance Json.DecodeJson APIResource where
  decodeJson = Json.Decode.Generic.genericDecodeJsonWith encodingOptions

-- | A resource that has to be fetched via donwloading the relevant file
data FileResource
  = BowerJson
  | SpagoDhall
  | PackagesDhall
  | PackageJson
  | LicenseFile

derive instance Eq FileResource
derive instance Generic FileResource _

instance Json.EncodeJson FileResource where
  encodeJson = Json.Encode.Generic.genericEncodeJsonWith encodingOptions

instance Json.DecodeJson FileResource where
  decodeJson = Json.Decode.Generic.genericDecodeJsonWith encodingOptions

fileResourcePath :: FileResource -> FilePath
fileResourcePath = case _ of
  BowerJson -> "bower.json"
  SpagoDhall -> "spago.dhall"
  PackagesDhall -> "packages.dhall"
  PackageJson -> "package.json"
  LicenseFile -> "LICENSE"
