module Test.Registry.Scripts.BowerImport.Error.Stats where

import Registry.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Map as Map
import Data.Newtype as Newtype
import Registry.Scripts.BowerImport.Error (ImportError(..), ManifestError(..), PackageFailures(..), RawPackageName(..), RawVersion(..), manifestErrorKey, printImportErrorKey, printManifestErrorKey)
import Registry.Scripts.BowerImport.Error.Stats (ProcessedPackageVersions)
import Registry.Scripts.BowerImport.Error.Stats as Stats
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

infixr 6 NonEmptyArray.cons' as :|

examplePackageResults :: ProcessedPackageVersions RawPackageName RawVersion Unit 
examplePackageResults = 
  { packages: 
      Map.fromFoldable 
        [ RawPackageName "pkg-a" /\ Map.fromFoldable [ RawVersion "1.0.0" /\ unit, RawVersion "1.0.1" /\ unit ]
        , RawPackageName "pkg-b" /\ Map.fromFoldable [ RawVersion "1.0.0" /\ unit, RawVersion "1.1.0" /\ unit, RawVersion "1.1.1" /\ unit ]
        ]
  , failures: exampleFailures
  }

exampleFailures :: PackageFailures
exampleFailures = PackageFailures $ 
  Map.fromFoldable 
    [ printImportErrorKey MissingBowerfile /\ missingBowerfileErrors
    , printImportErrorKey NoReleases /\ noReleasesErrors
    , printImportErrorKey NoManifests /\ noManifestErrors
    , manifestErrorKey /\ manifestErrors
    ]

  where 
    missingBowerfileErrors = Map.fromFoldable
      [ RawPackageName "pkg1" /\ errsByVersion threeMissingFileVersions 
      , RawPackageName "pkg2" /\ errByPackage MissingBowerfile
      ]

    noReleasesErrors = Map.fromFoldable
      [ RawPackageName "pkg2" /\ errByPackage NoReleases  -- dupe package, shouldn't add to count
      , RawPackageName "pkg3" /\ errByPackage NoReleases
      ]

    noManifestErrors = Map.fromFoldable 
      [ RawPackageName "pkg4" /\ errsByVersion twoMissingManifestVersions 
      , RawPackageName "pkg1" /\ errsByVersion (Map.fromFoldable [ RawVersion "1.2.1" /\ NoManifests ]) -- dupe package and version, neither should add to count
      ]

    manifestErrors = Map.fromFoldable 
      [ RawPackageName "pkg5" /\ errsByVersion twoVersionsWithManifestErrors ]


    threeMissingFileVersions = Map.fromFoldable 
      [ RawVersion "1.2.1" /\ MissingBowerfile 
      , RawVersion "1.2.2" /\ MissingBowerfile 
      , RawVersion "1.2.3" /\ MissingBowerfile 
      ]

    twoMissingManifestVersions = Map.fromFoldable 
      [ RawVersion "1.1.1" /\ NoManifests
      , RawVersion "1.2.1" /\ NoManifests -- dupe version, but SHOULD add to count, since it's from a different package 
      ]

    twoVersionsWithManifestErrors = Map.fromFoldable 
      [ RawVersion "1.0.0" /\ ManifestError twoManifestErrors 
      , RawVersion "2.0.0" /\ ManifestError threeManifestErrors 
      ]

    twoManifestErrors = MissingLicense :| [MissingName]
    threeManifestErrors = MissingLicense :| [BadVersion "x.y.z", InvalidDependencyNames ("doesn't" :| ["exist"])]

    errByPackage = Left
    errsByVersion = Right

exampleStats :: Stats.Stats
exampleStats = Stats.errorStats examplePackageResults

comparableMap :: forall k n v. Newtype k n => Ord n => Show n => Map k v -> Map n v 
comparableMap originalMap = 
  let kvPairs = (Map.toUnfoldable originalMap :: Array _) <#> \(key /\ val) -> (Newtype.unwrap key /\ val)
  in Map.fromFoldable kvPairs

assertMapsEqual :: forall k n v
  . Show n => Show v => Eq n => Eq v => Newtype k n => Ord n 
  => Map k v -> Map k v -> Aff Unit
assertMapsEqual = Assert.shouldEqual `on` comparableMap

errorStats :: Spec.Spec Unit
errorStats = do
  Spec.describe "count successes" do 
    Spec.it "sums the number of successful packages and versions" do 
      exampleStats.countOfPackageSuccesses `Assert.shouldEqual` 2
      exampleStats.countOfVersionSuccesses `Assert.shouldEqual` 5

  Spec.describe "count package failures" do
    Spec.it "sums the number of failed packages, regardless of how many failures per package" do
      exampleStats.countOfPackageFailures `Assert.shouldEqual` 5

  Spec.describe "count version failures" do
    Spec.it "sums the number of failed versions, regardless of how many failures per version" do
      exampleStats.countOfVersionFailures `Assert.shouldEqual` 7

  Spec.describe "count specific errors" do
    Spec.it "sums the number of each type of import, regardless of which packages or versions it occurred in" do
      exampleStats.countImportErrorsByErrorType `assertMapsEqual`
        Map.fromFoldable 
          [ (printImportErrorKey MissingBowerfile) /\ 4
          , (printImportErrorKey NoReleases) /\ 2
          , (printImportErrorKey NoManifests) /\ 3
          , manifestErrorKey /\ 2
          ]

    Spec.it "sums the number of each type of import, regardless of which packages or versions it occurred in" do
      exampleStats.countManifestErrorsByErrorType `assertMapsEqual`
        Map.fromFoldable 
          [ printManifestErrorKey MissingLicense /\ 2
          , printManifestErrorKey MissingName /\ 1
          , printManifestErrorKey (BadVersion "") /\ 1
          , printManifestErrorKey (InvalidDependencyNames (NonEmptyArray.singleton "")) /\ 1
          ]

  Spec.describe "pretty print stats" do
    Spec.it "prints a sorted list of all the collected stats" do
      Stats.prettyPrintStats exampleStats `Assert.shouldEqual`
        Foldable.intercalate "\n" 
          [ "Number of successful packages: " <> show exampleStats.countOfPackageSuccesses 
          , "Number of failed packages: " <> show exampleStats.countOfPackageFailures 
          , "Number of successful versions: " <> show exampleStats.countOfVersionSuccesses
          , "Number of failed versions: " <> show exampleStats.countOfVersionFailures
          , "Failures by error:"
          , "  missingBowerfile: 4" 
          , "  noManifests: 3"
          , "  manifestError: 2 (total packages/versions)"
          , "    missingLicense: 2"
          , "    badVersion: 1"
          , "    invalidDependencyNames: 1"
          , "    missingName: 1"
          , "  noReleases: 2"
          ]

