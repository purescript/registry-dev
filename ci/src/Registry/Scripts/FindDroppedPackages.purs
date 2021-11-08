module Registry.Scripts.FindDroppedPackages where

import Registry.Prelude

import Effect.Console (logShow)
import Data.Argonaut as Json
import Data.Array as Array
import Data.List as List
import Data.Map as Map
import Effect.Aff as Aff
import Registry.Scripts.LegacyImport.Error (ImportError, PackageFailures(..), RawPackageName(..), RawVersion(..))

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
            liftEffect $ logShow $ map (un RawPackageName) packagesThatWillBeDropped

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

type ExcludedPackages = Map RawPackageName (Either ImportError (Map RawVersion ImportError))

dropImportErrorKeys :: PackageFailures -> ExcludedPackages
dropImportErrorKeys =
  un PackageFailures
    >>> Map.values
    >>> List.foldl Map.union Map.empty

findPackagesThatWillBeDropped :: PackageSet -> ExcludedPackages -> Array RawPackageName
findPackagesThatWillBeDropped packageSet bowerExclusions =
  packageSet
    # un PackageSet
    # Map.toUnfoldable
    # Array.mapMaybe \(Tuple name { version }) -> willBeDropped name version
  where
  willBeDropped name version =
    case Map.lookup name bowerExclusions of
      Just (Left _) -> Just name
      Just (Right excludedVersions) ->
        case Map.lookup version excludedVersions of
          Just _ -> Just name
          Nothing -> Nothing
      Nothing -> Nothing
