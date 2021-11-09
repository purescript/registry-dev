module Registry.Scripts.FindDroppedPackages where

import Registry.Prelude

import Effect.Console (logShow)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Interpolate (i)
import Data.List as List
import Data.Map as Map
import Data.String.Utils (lines)
import Effect.Aff as Aff
import Registry.Scripts.LegacyImport.Error
  ( ImportError(..)
  , ImportErrorKey(..)
  , ManifestError(..)
  , PackageFailures(..)
  , RawPackageName(..)
  , RawVersion(..)
  )

newtype PackageSet = PackageSet (Map RawPackageName PackageSetPackage)

derive instance Newtype PackageSet _

instance Json.DecodeJson PackageSet where
  decodeJson json = do
    packagesObject :: Object PackageSetPackage <- Json.decodeJson json
    pure
      $ PackageSet
      $ objectToMap (Just <<< RawPackageName) packagesObject

type PackageSetPackage =
  { version :: RawVersion
  }

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
      packageSetPackagesFile <- readJsonFile "../../package-sets/packages.json"
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
  BadVersion version -> "bad version (" <> version <> ")"
  InvalidDependencyNames _ -> "invalid dependency names"
  BadDependencyVersions _ -> "bad dependency versions"

dropImportErrorKeys :: PackageFailures -> ExcludedPackages
dropImportErrorKeys =
  un PackageFailures
    >>> Map.values
    >>> List.foldl Map.union Map.empty

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
