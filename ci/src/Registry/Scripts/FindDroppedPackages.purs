module Registry.Scripts.FindDroppedPackages where

import Registry.Prelude

import Effect.Console (logShow)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Interpolate (i)
import Data.List as List
import Data.Map as Map
import Effect.Aff as Aff
import Registry.Scripts.LegacyImport.Error (ImportError, ImportErrorKey(..), PackageFailures(..), RawPackageName(..), RawVersion(..))
import Registry.Scripts.LegacyImport.Error as Error

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
              # traverse printMessage

printMessage :: DroppedPackage -> Effect Unit
printMessage droppedPackage = log message
  where
  name = un RawPackageName droppedPackage.name
  version = un RawVersion droppedPackage.version
  reason = un ImportErrorKey $ Error.printImportErrorKey droppedPackage.reason
  packageIdentifier = name <> " " <> version
  message = i packageIdentifier " will be dropped from the package set due to: " <> reason

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
