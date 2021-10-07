module Test.Registry.Scripts.LegacyImport.Stats where

import Registry.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable as Foldable
import Data.Lens (_1, _2, over, set, traversed)
import Data.Map as Map
import Foreign.Object as Object
import Foreign.SPDX (License)
import Foreign.SPDX as License
import Foreign.SemVer (SemVer, parseSemVer)
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Repo(..))
import Registry.Scripts.LegacyImport.Error (ImportError(..), ManifestError(..), PackageFailures(..), RawPackageName(..), RawVersion(..), manifestErrorKey, printImportErrorKey, printManifestErrorKey)
import Registry.Scripts.LegacyImport.Process (ProcessedPackageVersions)
import Registry.Scripts.LegacyImport.Stats (ErrorCounts(..))
import Registry.Scripts.LegacyImport.Stats as Stats
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
exampleFailures = PackageFailures $ Map.fromFoldable
  [ printImportErrorKey NoDependencyFiles /\ noDependencyFileErrors
  , printImportErrorKey (MalformedPackageName "Pkg1") /\ malformedPackageNameErrors
  , printImportErrorKey NoManifests /\ noManifestErrors
  , manifestErrorKey /\ manifestErrors
  ]
  where
  noDependencyFileErrors = Map.fromFoldable
    [ RawPackageName "pkg1" /\ errsByVersion threeMissingFileVersions
    , RawPackageName "pkg2" /\ errByPackage NoDependencyFiles
    ]

  malformedPackageNameErrors = Map.fromFoldable
    [ RawPackageName "pkg2" /\ errByPackage (MalformedPackageName "pkg2") -- dupe package, shouldn't add to count
    , RawPackageName "pkg3" /\ errByPackage (MalformedPackageName "pkg3")
    ]

  noManifestErrors = Map.fromFoldable
    [ RawPackageName "pkg4" /\ errsByVersion twoMissingManifestVersions
    , RawPackageName "pkg1" /\ errsByVersion (Map.fromFoldable [ RawVersion "1.2.1" /\ NoManifests ]) -- dupe package and version, neither should add to count
    ]

  manifestErrors = Map.fromFoldable
    [ RawPackageName "pkg5" /\ errsByVersion twoVersionsWithManifestErrors ]

  threeMissingFileVersions = Map.fromFoldable
    [ RawVersion "1.2.1" /\ NoDependencyFiles
    , RawVersion "1.2.2" /\ NoDependencyFiles
    , RawVersion "1.2.3" /\ NoDependencyFiles
    ]

  twoMissingManifestVersions = Map.fromFoldable
    [ RawVersion "1.1.1" /\ NoManifests
    , RawVersion "1.2.1" /\ NoManifests -- dupe version, but SHOULD add to count, since it's from a different package
    ]

  twoVersionsWithManifestErrors = Map.fromFoldable
    [ RawVersion "1.0.0" /\ ManifestError twoManifestErrors
    , RawVersion "2.0.0" /\ ManifestError threeManifestErrors
    ]

  twoManifestErrors = MissingLicense :| [ MissingName ]
  threeManifestErrors = MissingLicense :| [ BadVersion "x.y.z", InvalidDependencyNames ("doesn't" :| [ "exist" ]) ]

  errByPackage = Left
  errsByVersion = Right

exampleStats :: Stats.Stats
exampleStats = Stats.errorStats mockStats
  where
  mockLicense :: License
  mockLicense = unsafePartial $ fromJust $ hush $ License.parse "MIT"

  mockPackageName :: PackageName
  mockPackageName = unsafePartial $ fromJust $ hush $ PackageName.parse "foobarbaz"

  mockSemVer :: SemVer
  mockSemVer = unsafePartial $ fromJust $ parseSemVer "0.0.0"
  mockStats =
    { failures: examplePackageResults.failures
    , packages: Map.fromFoldable
        $ over (traversed <<< _2)
            ( \mp ->
                Map.fromFoldable
                  $ set (traversed <<< _2)
                      { license: mockLicense
                      , name: mockPackageName
                      , repository: Git { subdir: Nothing, url: "https://github.com/purescript/foobar" }
                      , targets: Object.empty
                      , version: mockSemVer
                      }
                  $ over (traversed <<< _1)
                      ( \raw ->
                          { semVer: mockSemVer
                          , original: raw
                          }
                      )
                  $ (identity :: Array ~> Array)
                  $ Map.toUnfoldable mp
            )
        $ over (traversed <<< _1)
            ( \raw ->
                { address:
                    { owner: "purescript"
                    , repo: "a-repo"
                    }
                , name: mockPackageName
                , original: raw
                }
            )
        $ ((Map.toUnfoldable :: _ -> Array _) examplePackageResults.packages)
    }

errCounts :: Int -> Int -> Int -> ErrorCounts
errCounts o p v = ErrorCounts { countOfOccurrences: o, countOfPackagesAffected: p, countOfVersionsAffected: v }

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
      exampleStats.countImportErrorsByErrorType `Assert.shouldEqual`
        Map.fromFoldable
          [ printImportErrorKey NoDependencyFiles /\ errCounts 4 2 3
          , printImportErrorKey (MalformedPackageName "Pkgs2") /\ errCounts 2 2 0
          , printImportErrorKey NoManifests /\ errCounts 3 2 3
          , manifestErrorKey /\ errCounts 2 1 2
          ]

    Spec.it "sums the number of each type of import, regardless of which packages or versions it occurred in" do
      exampleStats.countManifestErrorsByErrorType `Assert.shouldEqual`
        Map.fromFoldable
          [ printManifestErrorKey MissingLicense /\ errCounts 2 1 2
          , printManifestErrorKey MissingName /\ errCounts 1 1 1
          , printManifestErrorKey (BadVersion "") /\ errCounts 1 1 1
          , printManifestErrorKey (InvalidDependencyNames (NonEmptyArray.singleton "")) /\ errCounts 1 1 1
          ]

  Spec.describe "pretty print stats" do
    Spec.it "prints a sorted list of all the collected stats" do
      Stats.prettyPrintStats exampleStats `Assert.shouldEqual`
        Foldable.intercalate "\n"
          [ "Packages: " <> show exampleStats.totalPackages
              <> " total ("
              <> show exampleStats.countOfPackageSuccessesWithoutFailures
              <> " with successes, "
              <> show exampleStats.countOfPackageFailuresWithoutSuccesses
              <> " with failures)"
          , "Versions: " <> show exampleStats.totalVersions
              <> " total ("
              <> show exampleStats.countOfVersionSuccessesWithoutFailures
              <> " successful, "
              <> show exampleStats.countOfVersionFailuresWithoutSuccesses
              <> " failed)"
          , "Failures by error:"
          , "  noDependencyFiles: 4 occurrences (2 packages / 3 versions)"
          , "  noManifests: 3 occurrences (2 packages / 3 versions)"
          , "  malformedPackageName: 2 occurrences (2 packages / 0 versions)"
          , "  manifestError: 2 occurrences (1 packages / 2 versions)"
          , "    missingLicense: 2 occurrences (1 packages / 2 versions)"
          , "    badVersion: 1 occurrences (1 packages / 1 versions)"
          , "    invalidDependencyNames: 1 occurrences (1 packages / 1 versions)"
          , "    missingName: 1 occurrences (1 packages / 1 versions)"
          ]

