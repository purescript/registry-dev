module Registry.Scripts.BowerImport.Error where

import Registry.Prelude

import Data.Argonaut as Json
import Data.Argonaut.Decode.Generic as Json.Decode.Generic
import Data.Argonaut.Encode.Generic as Json.Encode.Generic
import Data.Argonaut.Types.Generic as Json.Generic
import Data.Generic.Rep (class Generic)
import Registry.PackageName (PackageName)
import Registry.Prelude as Maybe
import Safe.Coerce (coerce)

-- | A map of error types to package names to package versions, where failed
-- | versions contain rich information about why they failed.
newtype PackageFailures = PackageFailures (Map ImportErrorKey (Map RawPackageName (Map (Maybe RawVersion) ImportError)))
derive instance Newtype PackageFailures _

instance Json.EncodeJson PackageFailures where
  encodeJson failures =
    Json.encodeJson
      $ objectFromMap coerce
      $ map (objectFromMap coerce)
      $ map (map (objectFromMap (Maybe.fromMaybe "null" <<< coerce)))
      $ un PackageFailures failures

instance Json.DecodeJson PackageFailures where
  decodeJson json = do
    let parseTag tag = if tag == "null" then Nothing else Just (RawVersion tag)
    failuresObject :: Object (Object (Object ImportError)) <- Json.decodeJson json
    pure
      $ PackageFailures
      $ objectToMap (Just <<< ImportErrorKey)
      $ map (objectToMap (Just <<< RawPackageName))
      $ map (map (objectToMap (Just <<< parseTag))) failuresObject

-- | An import error printed as a key usable in a map
newtype ImportErrorKey = ImportErrorKey String
derive instance Newtype ImportErrorKey _
derive newtype instance Eq ImportErrorKey
derive newtype instance Ord ImportErrorKey

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

printImportErrorKey :: ImportError -> ImportErrorKey
printImportErrorKey = ImportErrorKey <<< case _ of
  InvalidGitHubRepo _ -> "invalidGitHubRepo"
  BadStatus _ -> "badStatus"
  NoReleases -> "noReleases"
  MalformedPackageName _ -> "malformedPackageName"
  MissingBowerfile -> "missingBowerfile"
  MalformedBowerJson _ -> "malformedBowerJson"
  NonRegistryDependencies _ -> "nonRegistryDependencies"
  NoManifests -> "noManifests"
  ManifestError _ -> "manifestError"

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

printManifestErrorKey :: ManifestError -> String
printManifestErrorKey = case _ of
  MissingName -> "missingName"
  MissingLicense -> "missingLicense"
  BadLicense _ -> "badLicense"
  BadVersion _ -> "badVersion"
  InvalidDependencyNames _ -> "invalidDependencyNames"
  BadDependencyVersions _ -> "badDependencyVersions"

encodingOptions :: Json.Generic.Encoding
encodingOptions = Json.Generic.defaultEncoding { unwrapSingleArguments = true }
