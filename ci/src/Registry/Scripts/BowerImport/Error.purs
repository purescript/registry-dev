module Registry.Scripts.BowerImport.Error where

import Registry.Prelude

import Data.Argonaut (JsonDecodeError, printJsonDecodeError)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.String as String
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName

-- | An error representing why a package version cannot be imported from the
-- | Bower registry.
data ImportError
  = InvalidGitHubRepo String
  | ExcludedPackage
  | NoReleases
  | MalformedPackageName String
  | MissingBowerfile
  | MalformedBowerJson JsonDecodeError
  | NonRegistryDependencies (NonEmptyArray String)
  | NoManifests
  | ManifestError (NonEmptyArray ManifestError)

printImportErrorKey :: ImportError -> String
printImportErrorKey = case _ of
  InvalidGitHubRepo _ -> "invalidGitHubRepo"
  ExcludedPackage -> "excludedPackage"
  NoReleases -> "noReleases"
  MalformedPackageName _ -> "malformedPackageName"
  MissingBowerfile -> "missingBowerfile"
  MalformedBowerJson _ -> "malformedBowerJson"
  NonRegistryDependencies _ -> "nonRegistryDependencies"
  NoManifests -> "noManifests"
  ManifestError errs ->
    "manifestError."
      <> String.joinWith "." (printManifestErrorKey <$> NEA.toArray errs)

printImportError :: ImportError -> String
printImportError = case _ of
  InvalidGitHubRepo err ->
    "Invalid GitHub repo: " <> err

  ExcludedPackage ->
    "Excluded package."

  NoReleases ->
    "No releases."

  MalformedPackageName err ->
    "Malformed name: " <> err

  MissingBowerfile ->
    "No bower file."

  MalformedBowerJson err ->
    "Malformed JSON: " <> printJsonDecodeError err

  NonRegistryDependencies deps ->
    "Non-registry dependencies: " <> String.joinWith ", " (NEA.toArray deps)

  NoManifests ->
    "No manifests produced"

  ManifestError err ->
    String.joinWith ", " $ map printManifestError $ NEA.toArray err

-- | An error representing why a Bowerfile cannot be migrated into a manifest.
data ManifestError
  = MissingName
  | MismatchedName { expected :: String, received :: String }
  | MissingLicense
  | BadLicense (Array String)
  | BadVersion String
  | InvalidDependencyNames (NonEmptyArray String)
  | BadDependencyVersions (NonEmptyArray { dependency :: PackageName, failedBounds :: String })

printManifestErrorKey :: ManifestError -> String
printManifestErrorKey = case _ of
  MissingName -> "missingName"
  MismatchedName _ -> "mismatchedName"
  MissingLicense -> "missingLicense"
  BadLicense _ -> "badLicense"
  BadVersion _ -> "badVersion"
  InvalidDependencyNames _ -> "invalidDependencyNames"
  BadDependencyVersions _ -> "badDependencyVersions"

printManifestError :: ManifestError -> String
printManifestError = case _ of
  MissingName ->
    "No 'name' field"

  MismatchedName { expected, received } ->
    "Bower file should have name purescript-"
      <> expected
      <> " but has name "
      <> received

  MissingLicense ->
    "No 'license' field"

  BadLicense err ->
    "Non-SPDX licenses: " <> String.joinWith ", " err

  BadVersion version ->
    "Invalid 'version' field: " <> version

  InvalidDependencyNames deps ->
    "Malformed depndency names: " <> String.joinWith ", " (NEA.toArray deps)

  BadDependencyVersions deps -> do
    let fromDep { dependency, failedBounds } = "(" <> PackageName.print dependency <> ": " <> failedBounds <> ")"
    "Bad dependency versions: " <> String.joinWith ", " (fromDep <$> NEA.toArray deps)
