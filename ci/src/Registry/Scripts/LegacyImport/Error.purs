module Registry.Scripts.LegacyImport.Error where

import Registry.Prelude

import Data.Interpolate (i)
import Registry.Json (class RegistryJson, (.:))
import Registry.Json as Json
import Registry.PackageName (PackageName)
import Registry.Types (RawPackageName(..), RawVersion(..))

-- | A map of error types to package names to package versions, where failed
-- | versions contain rich information about why they failed.
newtype PackageFailures = PackageFailures (Map ImportErrorKey (Map RawPackageName (Either ImportError (Map RawVersion ImportError))))

derive instance Newtype PackageFailures _
derive newtype instance RegistryJson PackageFailures

-- | An import error printed as a key usable in a map
newtype ImportErrorKey = ImportErrorKey String

derive instance Newtype ImportErrorKey _
derive newtype instance Eq ImportErrorKey
derive newtype instance Ord ImportErrorKey

instance Show ImportErrorKey where
  show (ImportErrorKey key) = i "(ImportErrorKey " key ")"

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

instance RegistryJson ImportError where
  encode = case _ of
    InvalidGitHubRepo repo ->
      Json.encode { tag: "InvalidGitHubRepo", value: repo }
    ResourceError error ->
      Json.encode { tag: "ResourceError", value: error }
    MalformedPackageName name ->
      Json.encode { tag: "MalformedPackageName", value: name }
    NoDependencyFiles ->
      Json.encode "NoDependencyFiles"
    NonRegistryDependencies array ->
      Json.encode { tag: "NonRegistryDependencies", value: array }
    NoManifests ->
      Json.encode "NoManifests"
    ManifestImportError array ->
      Json.encode { tag: "ManifestImportError", value: array }

  decode json = do
    let invalidGitHubRepo = tagged InvalidGitHubRepo "InvalidGitHubRepo"
    let resourceError = tagged ResourceError "ResourceError"
    let malformedPackageName = tagged MalformedPackageName "MalformedPackageName"
    let noDependencyFiles = nullary NoDependencyFiles "NoDependencyFiles"
    let nonRegistryDependencies = tagged NonRegistryDependencies "NonRegistryDependencies"
    let noManifests = nullary NoManifests "NoManifests"
    let manifestImportError = tagged ManifestImportError "ManifestImportError"
    invalidGitHubRepo
      <|> resourceError
      <|> malformedPackageName
      <|> noDependencyFiles
      <|> nonRegistryDependencies
      <|> noManifests
      <|> manifestImportError
    where
    nullary :: ImportError -> String -> Either String ImportError
    nullary ctor key = Json.decode json >>= case _ of
      str | str == key -> Right ctor
      _ -> Left ("Expected " <> key)

    tagged :: forall a. RegistryJson a => (a -> ImportError) -> String -> Either String ImportError
    tagged ctor key = do
      obj <- Json.decode json
      tag <- obj .: "tag"
      if tag == key then
        map ctor $ obj .: "value"
      else
        Left $ "Expected " <> tag

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

instance RegistryJson RequestError where
  encode = case _ of
    BadRequest ->
      Json.encode { tag: "BadRequest" }
    BadStatus error ->
      Json.encode { tag: "BadStatus ", value: error }
    DecodeError error ->
      Json.encode { tag: "DecodeError", value: error }

  decode json = do
    obj <- Json.decode json
    tag <- obj .: "tag"
    case tag of
      "BadRequest" -> Right BadRequest
      "BadStatus" -> BadStatus <$> obj .: "value"
      "DecodeError" -> DecodeError <$> obj .: "value"
      other -> Left $ "Expected RequestError: " <> other

-- | An error representing why a manifest could not be produced for this package
data ManifestError
  = MissingName
  | MissingLicense
  | BadLicense (Array String)
  | BadVersion String
  | InvalidDependencyNames (NonEmptyArray String)
  | BadDependencyVersions (NonEmptyArray { dependency :: PackageName, failedBounds :: String })

derive instance Eq ManifestError

instance RegistryJson ManifestError where
  encode = case _ of
    MissingName ->
      Json.encode "MissingName"
    MissingLicense ->
      Json.encode "MissingLicense"
    BadLicense array ->
      Json.encode { tag: "BadLicense", value: array }
    BadVersion version ->
      Json.encode { tag: "BadVersion", value: version }
    InvalidDependencyNames array ->
      Json.encode { tag: "InvalidDependencyNames", value: array }
    BadDependencyVersions array ->
      Json.encode { tag: "BadDependencyVersions", value: array }

  decode json = do
    let missingName = nullary MissingName "MissingName"
    let missingLicense = nullary MissingLicense "MissingLicense"
    let badLicense = tagged BadLicense "BadLicense"
    let badVersion = tagged BadVersion "BadVersion"
    let invalidDependencyNames = tagged InvalidDependencyNames "InvalidDependencyNames"
    let badDependencyVersions = tagged BadDependencyVersions "BadDependencyVersions"
    missingName
      <|> missingLicense
      <|> badLicense
      <|> badVersion
      <|> invalidDependencyNames
      <|> badDependencyVersions
    where
    nullary :: ManifestError -> String -> Either String ManifestError
    nullary ctor key = Json.decode json >>= case _ of
      str | str == key -> Right ctor
      _ -> Left ("Expected " <> key)

    tagged :: forall a. RegistryJson a => (a -> ManifestError) -> String -> Either String ManifestError
    tagged ctor key = do
      obj <- Json.decode json
      tag <- obj .: "tag"
      if tag == key then
        map ctor $ obj .: "value"
      else
        Left $ "Expected " <> tag

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

instance RegistryJson RemoteResource where
  encode = case _ of
    APIResource resource -> Json.encode resource
    FileResource resource -> Json.encode resource
  decode json = do
    let parseAPIResource = APIResource <$> Json.decode json
    let parseFileResource = FileResource <$> Json.decode json
    parseAPIResource <|> parseFileResource

-- | A resource that has to be fetched via an API
data APIResource = GitHubReleases

derive instance Eq APIResource

instance RegistryJson APIResource where
  encode GitHubReleases = Json.encode "GitHubReleases"
  decode = Json.decode >=> case _ of
    "GitHubReleases" -> Right GitHubReleases
    other -> Left $ "Expected APIResource: " <> other

-- | A resource that has to be fetched via downloading the relevant file
data FileResource
  = BowerJson
  | SpagoDhall
  | PackagesDhall
  | PackageJson
  | LicenseFile

derive instance Eq FileResource

instance RegistryJson FileResource where
  encode = Json.encode <<< fileResourcePath
  decode = Json.decode >=> case _ of
    "bower.json" -> Right BowerJson
    "spago.dhall" -> Right SpagoDhall
    "packages.dhall" -> Right PackagesDhall
    "package.json" -> Right PackageJson
    "LICENSE" -> Right LicenseFile
    other -> Left $ "Expected FileResource: " <> other

fileResourcePath :: FileResource -> FilePath
fileResourcePath = case _ of
  BowerJson -> "bower.json"
  SpagoDhall -> "spago.dhall"
  PackagesDhall -> "packages.dhall"
  PackageJson -> "package.json"
  LicenseFile -> "LICENSE"
