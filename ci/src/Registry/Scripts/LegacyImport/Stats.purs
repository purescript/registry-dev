module Registry.Scripts.LegacyImport.Stats
  ( errorStats
  , prettyPrintStats
  , logStats
  , Stats
  , ErrorCounts(..)
  ) where

import Registry.Prelude

import Control.Monad.Writer as Writer
import Data.Array as Array
import Data.Compactable (compact)
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Interpolate (i)
import Data.List as List
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Monoid.Additive (Additive(..))
import Data.Set as Set
import Registry.Schema (Manifest)
import Registry.Scripts.LegacyImport.Error (ImportError(..), ImportErrorKey(..), ManifestError, ManifestErrorKey(..), PackageFailures(..), manifestErrorKey, printManifestErrorKey)
import Registry.Scripts.LegacyImport.Process (NameAddressOriginal, ProcessedPackageVersions, VersionOriginal)
import Registry.Types (RawPackageName, RawVersion)
import Safe.Coerce (coerce)

newtype ErrorCounts = ErrorCounts
  { countOfOccurrences :: Int
  , countOfPackagesAffected :: Int
  , countOfVersionsAffected :: Int
  }

derive newtype instance Eq ErrorCounts
derive newtype instance Ord ErrorCounts
derive instance Generic ErrorCounts _

instance Semigroup ErrorCounts where
  append (ErrorCounts a) (ErrorCounts b) = ErrorCounts
    { countOfOccurrences: a.countOfOccurrences + b.countOfOccurrences
    , countOfPackagesAffected: a.countOfPackagesAffected + b.countOfPackagesAffected
    , countOfVersionsAffected: a.countOfVersionsAffected + b.countOfVersionsAffected
    }

instance Monoid ErrorCounts where
  mempty = ErrorCounts { countOfOccurrences: 0, countOfPackagesAffected: 0, countOfVersionsAffected: 0 }

instance Show ErrorCounts where
  show = genericShow

printErrorCounts :: ErrorCounts -> String
printErrorCounts (ErrorCounts { countOfPackagesAffected, countOfVersionsAffected }) =
  i (show countOfVersionsAffected) " versions across " (show countOfPackagesAffected) " packages"

type Stats =
  { totalPackages :: Int
  , totalVersions :: Int
  , countOfPackageSuccessesWithoutFailures :: Int
  , countOfPackageFailuresWithoutSuccesses :: Int
  , countOfVersionSuccessesWithoutFailures :: Int
  , countOfVersionFailuresWithoutSuccesses :: Int
  , countImportErrorsByErrorType :: Map ImportErrorKey ErrorCounts
  , countManifestErrorsByErrorType :: Map ManifestErrorKey ErrorCounts
  }

type VersionFailures = Map RawPackageName (Map RawVersion (Array ImportError))

countManifestErrors :: PackageFailures -> Map ManifestErrorKey ErrorCounts
countManifestErrors (PackageFailures failures) = case Map.lookup manifestErrorKey failures of
  Nothing -> Map.empty
  Just shouldBeManifestErrors -> do
    let
      extractManifestErrors = case _ of
        ManifestImportError errs -> List.fromFoldable errs
        _ -> List.Nil

      manifestErrors :: Map RawPackageName (Either (List ManifestError) (Map RawVersion (List ManifestError)))
      manifestErrors = shouldBeManifestErrors <#> case _ of
        Left errs -> Left $ extractManifestErrors errs
        Right versionErrs -> Right (extractManifestErrors <$> versionErrs)

      groupedErrors :: Map ManifestErrorKey { packageFailures :: Set RawPackageName, versionFailures :: Map RawPackageName Int }
      groupedErrors = do
        let
          processOnePackage (package /\ packageOrVersionFailures) = case packageOrVersionFailures of
            Left errs -> do
              let
                statsPerError =
                  { packageFailures: Set.fromFoldable [ package ]
                  , versionFailures: SemigroupMap Map.empty
                  }
              SemigroupMap $ Map.fromFoldable (errs <#> \err -> printManifestErrorKey err /\ statsPerError)
            Right (errsByVersion :: Map RawVersion (List ManifestError)) -> do
              let
                statsPerError =
                  { packageFailures: Set.empty
                  , versionFailures: SemigroupMap (Map.fromFoldable [ package /\ Additive 1 ])
                  }

                toSemigroupMap errs =
                  SemigroupMap $ Map.fromFoldable (errs <#> \err -> printManifestErrorKey err /\ statsPerError)

              fold (List.fromFoldable errsByVersion <#> toSemigroupMap)
        coerce $ Array.foldMap processOnePackage $ Map.toUnfoldable manifestErrors

    groupedErrors <#> \{ packageFailures, versionFailures } -> ErrorCounts $
      { countOfOccurrences: Set.size packageFailures + Foldable.sum versionFailures
      , countOfPackagesAffected: Set.size (Map.keys versionFailures <> packageFailures)
      , countOfVersionsAffected: Foldable.sum versionFailures
      }

errorStats :: ProcessedPackageVersions NameAddressOriginal VersionOriginal Manifest -> Stats
errorStats { packages: succeededPackages, failures: packageFailures@(PackageFailures failures) } =
  { totalPackages
  , totalVersions
  , countOfPackageSuccessesWithoutFailures
  , countOfPackageFailuresWithoutSuccesses
  , countOfVersionSuccessesWithoutFailures
  , countOfVersionFailuresWithoutSuccesses
  , countImportErrorsByErrorType
  , countManifestErrorsByErrorType
  }
  where
  rawSuccesses = Set.map _.original $ Map.keys succeededPackages
  rawFailures = Foldable.fold (map Map.keys failures)
  totalPackages = Set.size (rawSuccesses <> rawFailures)
  countOfPackageSuccessesWithoutFailures = Set.size $ Set.difference rawSuccesses rawFailures
  countOfPackageFailuresWithoutSuccesses = Set.size $ Set.difference rawFailures rawSuccesses

  rawSuccessVersions :: Set (Tuple RawPackageName RawVersion)
  rawSuccessVersions = (Set.map <<< map) _.original $ Foldable.fold
    $ map (Set.map <$> (Tuple <<< _.original <<< fst) <*> snd)
    $ (map <<< map) Map.keys ((Map.toUnfoldable :: _ -> Array _) succeededPackages)

  rawFailedVersions :: Set (Tuple RawPackageName RawVersion)
  rawFailedVersions = Foldable.fold
    $ map (Set.map <$> (Tuple <<< fst) <*> (Map.keys <<< snd))
    $ compact
    $ map sequence
    $ (map <<< map) hush
    $ Foldable.fold
    $ map ((Map.toUnfoldable) :: _ -> Array _) (Map.values failures)

  totalVersions = Set.size (rawSuccessVersions <> rawFailedVersions)
  countOfVersionSuccessesWithoutFailures = Set.size $ Set.difference rawSuccessVersions rawFailedVersions
  countOfVersionFailuresWithoutSuccesses = Set.size $ Set.difference rawFailedVersions rawSuccessVersions

  countImportErrorsByErrorType = do
    let
      countFailuresForPackage :: Either ImportError (Map RawVersion ImportError) -> ErrorCounts
      countFailuresForPackage = case _ of
        Left _ -> ErrorCounts { countOfPackagesAffected: 1, countOfOccurrences: 1, countOfVersionsAffected: 0 }
        Right versionErrors -> ErrorCounts
          { countOfOccurrences: Map.size versionErrors
          , countOfVersionsAffected: Map.size versionErrors
          , countOfPackagesAffected: 1
          }

      countFailuresByPackage :: Map RawPackageName (Either ImportError (Map RawVersion ImportError)) -> ErrorCounts
      countFailuresByPackage = Foldable.foldMap countFailuresForPackage

    countFailuresByPackage <$> failures

  countManifestErrorsByErrorType = countManifestErrors packageFailures

prettyPrintStats :: Stats -> String
prettyPrintStats stats =
  Foldable.intercalate "\n" $
    fold
      [ [ "Packages: "
            <> show stats.totalPackages
            <> " total ("
            <> show stats.countOfPackageSuccessesWithoutFailures
            <> " totally succeeded, "
            <> show (stats.totalPackages - stats.countOfPackageSuccessesWithoutFailures - stats.countOfPackageFailuresWithoutSuccesses)
            <> " partially succeeded, "
            <> show stats.countOfPackageFailuresWithoutSuccesses
            <> " totally failed)"
        , "Versions: "
            <> show stats.totalVersions
            <> " total ("
            <> show stats.countOfVersionSuccessesWithoutFailures
            <> " totally succeeded, "
            <> show (stats.totalVersions - stats.countOfVersionSuccessesWithoutFailures - stats.countOfVersionFailuresWithoutSuccesses)
            <> " partially succeeded, "
            <> show stats.countOfVersionFailuresWithoutSuccesses
            <> " totally failed)"
        , "Failures by error:"
        ]
      , sortedErrors
      ]

  where
  sortedErrors = writeError <$> sortValues stats.countImportErrorsByErrorType

  sortValues :: forall k v. Ord v => Map k v -> Array (Tuple k v)
  sortValues = Array.sortBy (flip compare `on` snd) <<< Map.toUnfoldable

  writeError = Foldable.intercalate "\n" <<< Writer.execWriter <<< case _ of
    (key@(ImportErrorKey importErr) /\ importErrCounts)
      | key == manifestErrorKey -> do
          Writer.tell [ i "  " importErr ": " (printErrorCounts importErrCounts) ]
          for_
            (sortValues stats.countManifestErrorsByErrorType)
            \(ManifestErrorKey manifestErr /\ manifestErrCounts) ->
              Writer.tell [ i "    " manifestErr ": " (printErrorCounts manifestErrCounts) ]
      | otherwise ->
          Writer.tell [ i "  " importErr ": " (printErrorCounts importErrCounts) ]

logStats :: forall m. MonadEffect m => Stats -> m Unit
logStats = log <<< prettyPrintStats
