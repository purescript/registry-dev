module Registry.Scripts.BowerImport.Error where

import Registry.Prelude

import Data.Argonaut (JsonDecodeError, printJsonDecodeError)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.String as String
import Foreign.GitHub as GitHub
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName

-- | An error representing why a package version cannot be imported from the
-- | Bower registry.
data ImportError
  = NotOnGitHub
  | MalformedPackageName String
  | MissingBowerfile GitHub.Tag
  | MalformedBowerJson GitHub.Tag JsonDecodeError
  | InvalidDependencyNames GitHub.Tag (NonEmptyArray String)
  | NonRegistryDependencies GitHub.Tag (NonEmptyArray PackageName)
  | NoManifests
  | ManifestError GitHub.Tag (NonEmptyArray ManifestError)

tagFromError :: ImportError -> Maybe GitHub.Tag
tagFromError = case _ of
  NotOnGitHub -> Nothing
  MalformedPackageName _ -> Nothing
  MissingBowerfile tag -> Just tag
  MalformedBowerJson tag _ -> Just tag
  InvalidDependencyNames tag _ -> Just tag
  NonRegistryDependencies tag _ -> Just tag
  NoManifests -> Nothing
  ManifestError tag _ -> Just tag

printImportErrorKey :: ImportError -> String
printImportErrorKey = case _ of
  NotOnGitHub -> "notOnGitHub"
  MalformedPackageName _ -> "malformedPackageName"
  MissingBowerfile _ -> "missingBowerfile"
  MalformedBowerJson _ _ -> "malformedBowerJson"
  InvalidDependencyNames _ _ -> "invalidDependencyNames"
  NonRegistryDependencies _ _ -> "nonRegistryDependencies"
  NoManifests -> "noManifests"
  ManifestError _ errs ->
    "manifestError."
      <> String.joinWith "." (printManifestErrorKey <$> NEA.toArray errs)

printImportError :: ImportError -> String
printImportError = case _ of
  NotOnGitHub ->
    "Not on GitHub."

  MalformedPackageName err ->
    "Malformed name: " <> err

  MissingBowerfile _ ->
    "No bower file."

  MalformedBowerJson _ err ->
    "Malformed JSON:" <> printJsonDecodeError err

  InvalidDependencyNames _ deps ->
    "Malformed depndency names: " <> String.joinWith ", " (NEA.toArray deps)

  NonRegistryDependencies _ deps ->
    "Non-registry dependencies: " <> String.joinWith ", " (PackageName.print <$> NEA.toArray deps)

  NoManifests ->
    "No manifests produced"

  ManifestError _ err -> case NEA.toArray err of
    [ one ] -> printManifestError one
    many -> String.joinWith ", " (map printManifestError many)

-- | An error representing why a Bowerfile cannot be migrated into a manifest.
data ManifestError
  = MissingName
  | MismatchedName { expected :: PackageName, received :: String }
  | MissingLicense
  | BadLicense (Array String)
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
    "No 'name' field"

  MismatchedName { expected, received } ->
    "Bower file should have name purescript-"
      <> PackageName.print expected
      <> " but has name "
      <> received

  MissingLicense ->
    "No 'license' field"

  BadLicense err ->
    "Non-SPDX licenses: " <> String.joinWith ", " err

  BadVersion version ->
    "Invalid 'version' field: " <> version

  BadDependencyVersions deps -> do
    let fromDep { dependency, failedBounds } = "(" <> PackageName.print dependency <> ": " <> failedBounds <> ")"
    "Bad dependency versions: " <> String.joinWith ", " (fromDep <$> NEA.toArray deps)
