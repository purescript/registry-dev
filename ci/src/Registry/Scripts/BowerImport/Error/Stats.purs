module Registry.Scripts.BowerImport.Error.Stats 
  ( errorStats
  , prettyPrintStats
  , logStats
  , Stats
  , ProcessedPackages
  , ProcessedPackageVersions
  ) where

import Registry.Prelude

import Control.Monad.Writer as Writer
import Data.Array as Array
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Interpolate (i)
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Map (SemigroupMap)
import Data.Map as Map
import Data.Ordering as Ordering
import Data.Set as Set
import Registry.Scripts.BowerImport.Error (ImportError(..), ImportErrorKey(..), ManifestError, ManifestErrorKey(..), PackageFailures(..), RawPackageName, RawVersion, manifestErrorKey, printManifestErrorKey)
import Safe.Coerce (coerce)

type ProcessedPackages k a =
  { failures :: PackageFailures
  , packages :: Map k a
  }

type ProcessedPackageVersions k1 k2 a = ProcessedPackages k1 (Map k2 a)

type Stats = 
  { countOfPackageSuccesses :: Int
  , countOfPackageFailures :: Int
  , countOfVersionSuccesses :: Int
  , countOfVersionFailures :: Int
  , countImportErrorsByErrorType :: Map ImportErrorKey Int
  , countManifestErrorsByErrorType :: Map ManifestErrorKey Int
  }

type VersionFailures = Map RawPackageName (Map RawVersion (Array ImportError))

versionFailuresFromPackageFailures :: PackageFailures -> VersionFailures
versionFailuresFromPackageFailures (PackageFailures failures) = 
  let 
    onlyVersionFailures :: List (Map RawPackageName (Map RawVersion ImportError))
    onlyVersionFailures = Map.values failures # map (Map.mapMaybe hush)

    semigroupVersionFailures :: List (SemigroupMap RawPackageName (SemigroupMap RawVersion (Array ImportError)))
    semigroupVersionFailures = coerce (map (map (map Array.singleton)) onlyVersionFailures)

  in coerce (fold semigroupVersionFailures)

errorStats :: forall package version a. ProcessedPackageVersions package version a -> Stats
errorStats {packages: succeededPackages, failures: packageFailures@(PackageFailures failures)} = 
  { countOfPackageSuccesses
  , countOfPackageFailures
  , countOfVersionSuccesses
  , countOfVersionFailures
  , countImportErrorsByErrorType
  , countManifestErrorsByErrorType 
  } 
  where 
    countOfPackageSuccesses = Map.size succeededPackages 
    countOfVersionSuccesses = Foldable.sum $ map Map.size succeededPackages

    countOfPackageFailures = 
      let packages = fold $ map Map.keys $ Map.values failures
      in Set.size packages

    countOfVersionFailures = 
      Foldable.sum $ map Map.size $ versionFailuresFromPackageFailures packageFailures

    countImportErrorsByErrorType = 
      let 
        countFailuresForPackage :: Either ImportError (Map RawVersion ImportError) -> Int
        countFailuresForPackage = either (const 1) Map.size
      in (Foldable.sum <<< map countFailuresForPackage) <$> failures

    countManifestErrorsByErrorType = case Map.lookup manifestErrorKey failures of 
      Nothing -> Map.empty
      Just manifestErrors -> 
        let 
          allManifestErrors :: List ManifestError
          allManifestErrors = 
            let 
              allImportErrors = Map.values manifestErrors >>= either List.singleton List.fromFoldable
              extractManifestErrors = case _ of 
                ManifestError errs -> Just $ List.fromFoldable errs
                _ -> Nothing
            in allImportErrors # List.mapMaybe extractManifestErrors # join
        in 
          allManifestErrors
          # List.sortBy (compare `on` printManifestErrorKey) 
          # List.groupBy (eq `on` printManifestErrorKey) 
          # map (\v -> printManifestErrorKey (NonEmptyList.head v) /\ NonEmptyList.length v)
          # Map.fromFoldable


prettyPrintStats :: Stats -> String
prettyPrintStats stats = 
  Foldable.intercalate "\n"
    ( [ "Number of successful packages: " <> show stats.countOfPackageSuccesses 
      , "Number of failed packages: " <> show stats.countOfPackageFailures 
      , "Number of successful versions: " <> show stats.countOfVersionSuccesses
      , "Number of failed versions: " <> show stats.countOfVersionFailures
      , "Failures by error:"
      ]
      <>
      sortedErrors
    )

  where 
    sortedErrors = writeError <$> sortValues stats.countImportErrorsByErrorType
      
    sortValues :: forall k v. Ord v => Map k v -> Array (Tuple k v)
    sortValues = Array.sortBy (flip compare `on` snd) <<< Map.toUnfoldable

    writeError = Foldable.intercalate "\n" <<< Writer.execWriter <<< case _ of 
      (key@(ImportErrorKey keyStr) /\ errCount) 
        | key == manifestErrorKey -> do
          Writer.tell [ i"  "keyStr": "(show errCount)" (total packages/versions)" ]
          for_ 
            (sortValues stats.countManifestErrorsByErrorType) 
            \(ManifestErrorKey err /\ count) -> Writer.tell [ i"    "err": "(show count) ]
        | otherwise -> 
          Writer.tell [ i"  "keyStr": "(show errCount) ]


logStats :: forall m. MonadEffect m => Stats -> m Unit
logStats = log <<< prettyPrintStats 
