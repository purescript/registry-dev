module Registry.Scripts.FindDroppedPackages where

import Registry.Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Interpolate (i)
import Data.List as List
import Data.Map as Map
import Data.Array.NonEmpty as NEA
import Data.String.Utils (lines)
import Effect.Aff as Aff
import Registry.PackageName as PackageName
import Registry.Scripts.LegacyImport.Error
  ( ImportError(..)
  , ManifestError(..)
  , PackageFailures(..)
  , RawPackageName(..)
  , RawVersion(..)
  )

-- | A PureScript package set.
newtype PackageSet = PackageSet (Map RawPackageName PackageSetPackage)

derive instance Newtype PackageSet _

instance Json.DecodeJson PackageSet where
  decodeJson json = do
    packagesObject :: Object PackageSetPackage <- Json.decodeJson json
    pure
      $ PackageSet
      $ objectToMap (Just <<< RawPackageName) packagesObject

-- | A package in the package set.
type PackageSetPackage =
  { version :: RawVersion
  }

-- | A package that will be dropped from the package set.
type DroppedPackage =
  { name :: RawPackageName
  , version :: RawVersion
  , reason :: ImportError
  }

type ExcludedPackages = Map RawPackageName (Either ImportError (Map RawVersion ImportError))

main :: Effect Unit
main = Aff.launchAff_ do
  bowerExclusionsFile <- readJsonFile "bower-exclusions.json"
  case bowerExclusionsFile of
    Left err -> do
      let decodeError = "Decoding bower-exclusions.json failed with error:\n\n" <> Json.printJsonDecodeError err
      throwError $ Aff.error decodeError
    Right bowerExclusions -> do
      packageSetPackagesFile <- readJsonFile "package-set-packages.json"
      case packageSetPackagesFile of
        Left err -> do
          let decodeError = "Decoding packages.json failed with error:\n\n" <> Json.printJsonDecodeError err
          throwError $ Aff.error decodeError
        Right packages ->
          let
            packagesThatWillBeDropped = findPackagesThatWillBeDropped packages $ dropImportErrorKeys bowerExclusions
          in
            liftEffect $ packagesThatWillBeDropped
              # map printMessage
              # intercalate "\n\n"
              # log

printMessage :: DroppedPackage -> String
printMessage droppedPackage = message
  where
  name = un RawPackageName droppedPackage.name
  version = un RawVersion droppedPackage.version
  reason = printDroppedReason droppedPackage.reason
  packageIdentifier = name <> " " <> version
  message = i packageIdentifier " will be dropped from the package set due to:\n\t" <> reason

printDroppedReason :: ImportError -> String
printDroppedReason = case _ of
  InvalidGitHubRepo _ -> "invalid GitHub repo"
  ResourceError _ -> "resource error"
  MalformedPackageName _ -> "malformed package name"
  NoDependencyFiles -> "no dependency files"
  NonRegistryDependencies _ -> "non-registry dependencies"
  NoManifests -> "no manifests"
  ManifestError manifestErrors ->
    manifestErrors
      # map printManifestError
      # intercalate ", "

printManifestError :: ManifestError -> String
printManifestError = case _ of
  MissingName -> "missing name"
  MissingLicense -> "missing license"
  BadLicense licenses ->
    "bad license:\n" <>
      ( intercalate "\n" $ map ("\t\t" <> _)
          $ Array.concatMap lines licenses
      )
  BadVersion version -> "bad version: " <> version
  InvalidDependencyNames dependencyNames ->
    "invalid dependency names:\n" <> intercalate "\n\t\t" dependencyNames
  BadDependencyVersions badVersions ->
    "bad dependency versions:\n" <>
      ( intercalate "\n" $ map ("\t\t" <> _)
          $ Array.concatMap lines
          $ map printBadDependencyVersion
          $ NEA.toArray badVersions
      )

  where
  printBadDependencyVersion { dependency, failedBounds } =
    i "Dependency: " (PackageName.print dependency) "\nFailed bounds: " failedBounds

-- | Drops the import keys from the `PackageFailures` collection, as we don't
-- | need the groupings.
dropImportErrorKeys :: PackageFailures -> ExcludedPackages
dropImportErrorKeys =
  un PackageFailures
    >>> Map.values
    >>> List.foldl Map.union Map.empty

-- | Returns an array of all packages that will be dropped from the package set
-- | based on the packages currently excluded from the registry.
findPackagesThatWillBeDropped :: PackageSet -> ExcludedPackages -> Array DroppedPackage
findPackagesThatWillBeDropped packageSet bowerExclusions =
  packageSet
    # un PackageSet
    # Map.toUnfoldable
    # Array.mapMaybe \(Tuple name { version }) -> willBeDropped name version
  where
  willBeDropped name version =
    case Map.lookup name bowerExclusions of
      Just (Left reason) -> Just { name, version, reason }
      Just (Right excludedVersions) ->
        case Map.lookup version excludedVersions of
          Just reason -> Just { name, version, reason }
          Nothing -> Nothing
      Nothing -> Nothing
