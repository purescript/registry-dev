module Registry.Scripts.FindDroppedPackages where

import Registry.Prelude

import Effect.Console (logShow)
import Data.Argonaut as Json
import Effect.Aff as Aff
import Registry.Scripts.LegacyImport.Error (PackageFailures(..), RawPackageName(..), RawVersion(..))

main :: Effect Unit
main = Aff.launchAff_ do
  bowerExclusionsFile <- readJsonFile "bower-exclusions.json"
  case bowerExclusionsFile of
    Left err -> do
      let decodeError = "Decoding bower-exlusions.json failed with error:\n\n" <> Json.printJsonDecodeError err
      throwError $ Aff.error decodeError
    Right bowerExclusions -> do
      packageSetPackagesFile <- readJsonFile "../../package-sets/packages.json"
      case packageSetPackagesFile of
        Left err -> do
          let decodeError = "Decoding packages.json failed with error:\n\n" <> Json.printJsonDecodeError err
          throwError $ Aff.error decodeError
        Right packages ->
          let
            packagesThatWillBeDropped = findPackagesThatWillBeDropped packages bowerExclusions
          in
            pure unit

-- pure unit

newtype PackageSet = PackageSet (Map RawPackageName PackageSetPackage)

instance Json.DecodeJson PackageSet where
  decodeJson json = do
    packagesObject :: Object PackageSetPackage <- Json.decodeJson json
    pure
      $ PackageSet
      $ objectToMap (Just <<< RawPackageName) packagesObject

-- type PackageSetJson = Map String { version :: String }

-- type PackageSetJson = Map RawPackageName PackageSetPackage

type PackageSetPackage =
  { version :: RawVersion
  }

findPackagesThatWillBeDropped :: PackageSet -> PackageFailures -> Array String
findPackagesThatWillBeDropped packageSet bowerExclusions = []
-- bowerExclusions
--   # un PackageFailures
--   # Map.values
--   # List.filter \package

-- willBeDropped :: PackageSetJson -> Either RawPackageName