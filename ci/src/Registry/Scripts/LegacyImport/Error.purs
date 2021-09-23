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

-- | An error representing why a package version cannot be imported from the
-- | Bower registry.
data ImportError
  = InvalidGitHubRepo String
  | BadStatus Int
  | NoReleases
  | MalformedPackageName String
  | MissingBowerfile
  | MalformedBowerJson { error :: String, contents :: String }
  | NonRegistryDependencies (NonEmptyArray RawPackageName)
  | NoManifests
  | ManifestError (NonEmptyArray ManifestError)

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
  BadStatus _ -> ImportErrorKey "badStatus"
  NoReleases -> ImportErrorKey "noReleases"
  MalformedPackageName _ -> ImportErrorKey "malformedPackageName"
  MissingBowerfile -> ImportErrorKey "missingBowerfile"
  MalformedBowerJson _ -> ImportErrorKey "malformedBowerJson"
  NonRegistryDependencies _ -> ImportErrorKey "nonRegistryDependencies"
  NoManifests -> ImportErrorKey "noManifests"
  ManifestError _ -> manifestErrorKey

-- | An error representing why a Bowerfile cannot be migrated into a manifest.
data ManifestError
  = MissingName
  | MissingLicense
  | BadLicense (Array String)
  | BadVersion String
  | InvalidDependencyNames (NonEmptyArray String)
  | BadDependencyVersions (NonEmptyArray { dependency :: PackageName, failedBounds :: String })

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
