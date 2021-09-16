module Registry.Scripts.BowerImport.Stats
  ( errorStats
  , prettyPrintStats
  , logStats
  , Stats
  , ErrorCounts(..)
  ) where

import Registry.Prelude

import Control.Monad.Writer as Writer
import Data.Array as Array
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Interpolate (i)
import Data.List as List
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Monoid.Additive as Monoid
import Data.Set as Set
import Registry.Scripts.BowerImport.Error (ImportError(..), ImportErrorKey(..), ManifestError, ManifestErrorKey(..), PackageFailures(..), RawPackageName, RawVersion, manifestErrorKey, printManifestErrorKey)
import Registry.Scripts.BowerImport.Process (ProcessedPackageVersions)
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
printErrorCounts (ErrorCounts { countOfOccurrences, countOfPackagesAffected, countOfVersionsAffected }) =
  i (show countOfOccurrences) " occurrences (" (show countOfPackagesAffected) " packages / " (show countOfVersionsAffected) " versions)"

type Stats =
  { countOfPackageSuccesses :: Int
  , countOfPackageFailures :: Int
  , countOfVersionSuccesses :: Int
  , countOfVersionFailures :: Int
  , countImportErrorsByErrorType :: Map ImportErrorKey ErrorCounts
  , countManifestErrorsByErrorType :: Map ManifestErrorKey ErrorCounts
  }

type VersionFailures = Map RawPackageName (Map RawVersion (Array ImportError))

versionFailuresFromPackageFailures :: PackageFailures -> VersionFailures
versionFailuresFromPackageFailures (PackageFailures failures) =
  let
    onlyVersionFailures :: List (Map RawPackageName (Map RawVersion ImportError))
    onlyVersionFailures = Map.values failures # map (Map.mapMaybe hush)

    semigroupVersionFailures :: List (SemigroupMap RawPackageName (SemigroupMap RawVersion (Array ImportError)))
    semigroupVersionFailures = coerce (map (map (map Array.singleton)) onlyVersionFailures)

  in
    coerce (fold semigroupVersionFailures)

countManifestErrors :: PackageFailures -> Map ManifestErrorKey ErrorCounts
countManifestErrors (PackageFailures failures) = case Map.lookup manifestErrorKey failures of
  Nothing -> Map.empty
  Just shouldBeManifestErrors ->
    let
      extractManifestErrors = case _ of
        ManifestError errs -> List.fromFoldable errs
        _ -> List.Nil

      manifestErrors :: Map RawPackageName (Either (List ManifestError) (Map RawVersion (List ManifestError)))
      manifestErrors = shouldBeManifestErrors <#> case _ of
        Left errs -> Left $ extractManifestErrors errs
        Right versionErrs -> Right (extractManifestErrors <$> versionErrs)

      groupedErrors :: Map ManifestErrorKey { packageFailures :: Set RawPackageName, versionFailures :: Map RawPackageName Int }
      groupedErrors =
        let
          processOnePackage (package /\ packageOrVersionFailures) = case packageOrVersionFailures of
            Left errs ->
              let
                statsPerError =
                  { packageFailures: Set.fromFoldable [ package ]
                  , versionFailures: SemigroupMap Map.empty
                  }
              in
                SemigroupMap $ Map.fromFoldable (errs <#> \err -> printManifestErrorKey err /\ statsPerError)
            Right (errsByVersion :: Map RawVersion (List ManifestError)) ->
              let
                statsPerError =
                  { packageFailures: Set.empty
                  , versionFailures: SemigroupMap (Map.fromFoldable [ package /\ Monoid.Additive 1 ])
                  }

                toSemigroupMap errs =
                  SemigroupMap $ Map.fromFoldable (errs <#> \err -> printManifestErrorKey err /\ statsPerError)
              in
                fold (List.fromFoldable errsByVersion <#> toSemigroupMap)
        in
          coerce $ Array.foldMap processOnePackage $ Map.toUnfoldable manifestErrors

    in
      groupedErrors
        <#> \{ packageFailures, versionFailures } ->
          ErrorCounts $
            { countOfOccurrences: Set.size packageFailures + Foldable.sum versionFailures
            , countOfPackagesAffected: Set.size (Map.keys versionFailures <> packageFailures)
            , countOfVersionsAffected: Foldable.sum versionFailures
            }

errorStats :: forall package version a. ProcessedPackageVersions package version a -> Stats
errorStats { packages: succeededPackages, failures: packageFailures@(PackageFailures failures) } =
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
    let
      packages = fold $ map Map.keys $ Map.values failures
    in
      Set.size packages

  countOfVersionFailures =
    Foldable.sum $ map Map.size $ versionFailuresFromPackageFailures packageFailures

  countImportErrorsByErrorType =
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
      countFailuresByPackage = fold <<< map countFailuresForPackage
    in
      countFailuresByPackage <$> failures

  countManifestErrorsByErrorType = countManifestErrors packageFailures

prettyPrintStats :: Stats -> String
prettyPrintStats stats =
  Foldable.intercalate "\n" $
    fold
      [ [ "Number of successful packages: " <> show stats.countOfPackageSuccesses
        , "Number of failed packages: " <> show stats.countOfPackageFailures
        , "Number of successful versions: " <> show stats.countOfVersionSuccesses
        , "Number of failed versions: " <> show stats.countOfVersionFailures
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
