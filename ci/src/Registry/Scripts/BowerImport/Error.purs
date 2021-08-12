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
  = NotOnGitHub
  | MissingBowerfile
  | MalformedBowerJson JsonDecodeError
  | InvalidDependencyNames (NonEmptyArray String)
  | NonRegistryDependencies (NonEmptyArray PackageName)
  | NoManifests
  | ManifestError ManifestError

printImportErrorKey :: ImportError -> String
printImportErrorKey = case _ of
  NotOnGitHub -> "notOnGitHub"
  MissingBowerfile -> "missingBowerfile"
  MalformedBowerJson _ -> "malformedBowerJson"
  InvalidDependencyNames _ -> "invalidDependencyNames"
  NonRegistryDependencies _ -> "nonRegistryDependencies"
  NoManifests -> "noManifests"
  ManifestError err -> "manifestError." <> printManifestErrorKey err

printImportError :: ImportError -> String
printImportError = case _ of
  NotOnGitHub ->
    "Not available on GitHub."

  MissingBowerfile ->
    "Missing bower file."

  MalformedBowerJson err ->
    "Malformed bower file:\n\n" <> printJsonDecodeError err

  InvalidDependencyNames deps ->
    "Bower file contains dependencies with malformed names:\n\n"
      <> String.joinWith ", " (NEA.toArray deps)

  NonRegistryDependencies deps ->
    "Bower file contains dependencies not in the registry:\n\n"
      <> String.joinWith ", " (PackageName.print <$> NEA.toArray deps)

  NoManifests ->
    "No valid manifests could be produced for this package."

  ManifestError err ->
    printManifestError err

-- | An error representing why a Bowerfile cannot be migrated into a manifest.
data ManifestError
  = MissingName
  | MismatchedName { expected :: PackageName, received :: String }
  | MissingLicense
  | BadLicense String
  | BadVersion String
  | BadDependencyVersions (NonEmptyArray { dependency :: PackageName, failedBounds :: String })

printManifestErrorKey :: ManifestError -> String
printManifestErrorKey = case _ of
  MissingName -> "missingName"
  MismatchedName _ -> "mismatchedName"
  MissingLicense -> "missingLicense"
  BadLicense _ -> "badLicense"
  BadVersion _ -> "badVersion"
  BadDependencyVersions _ -> "badDependencyVersions"

printManifestError :: ManifestError -> String
printManifestError = case _ of
  MissingName ->
    "Bower file does not contain a 'name' field."

  MismatchedName { expected, received } ->
    "Bower file name does not match package name. Expected '"
      <> PackageName.print expected
      <> "' but received '"
      <> received
      <> "'."

  MissingLicense ->
    "Bower file does not contain a 'license' field."

  BadLicense err ->
    "Bower file contains non-SPDX license:\n\n" <> err

  BadVersion version ->
    "Bower file declares an invalid version:\n\n" <> version

  BadDependencyVersions deps -> do
    let fromDep { dependency, failedBounds } = PackageName.print dependency <> ": " <> failedBounds
    "Bower file declares one or more dependencies with invalid version bounds:\n\n"
      <> String.joinWith "\n" (fromDep <$> NEA.toArray deps)
