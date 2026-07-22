module Test.Registry.Scripts.Main (main) where

import Registry.App.Prelude

import Data.DateTime (DateTime)
import Data.Map as Map
import Data.Time.Duration (Hours(..))
import Registry.Metadata (Metadata)
import Registry.Metadata as Metadata
import Registry.PackageName (PackageName)
import Registry.PackageSet (PackageSet)
import Registry.PackageSet as PackageSet
import Registry.Scripts.PackageSetUpdater as PackageSetUpdater
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Registry.Version (Version)
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Spec.describe "PackageSetUpdater candidate selection" do
    Spec.it "finds all newer versions of packages already in the package set" do
      let
        metadata = Map.fromFoldable
          [ Utils.unsafeMetadata "prelude" [ Tuple "1.0.0" [ "0.15.10" ], Tuple "2.0.0" [ "0.15.10" ] ]
          , Utils.unsafeMetadata "effect" [ Tuple "2.0.0" [ "0.15.10" ] ]
          , Utils.unsafeMetadata "older" [ Tuple "1.0.0" [ "0.15.10" ] ]
          , Utils.unsafeMetadata "never-in-set" [ Tuple "1.0.0" [ "0.15.10" ] ]
          ]
        packageSet = mkPackageSet $ Map.fromFoldable
          [ Tuple (Utils.unsafePackageName "prelude") (Utils.unsafeVersion "1.0.0")
          , Tuple (Utils.unsafePackageName "effect") (Utils.unsafeVersion "2.0.0")
          , Tuple (Utils.unsafePackageName "older") (Utils.unsafeVersion "2.0.0")
          ]
        expected = Map.singleton (Utils.unsafePackageName "prelude") (Utils.unsafeVersion "2.0.0")

      PackageSetUpdater.selectPackageSetCandidates PackageSetUpdater.AllPending packageSet metadata
        `Assert.shouldEqual` expected

    Spec.it "preserves recent candidate selection for new packages and upgrades" do
      let
        oldTime = Utils.unsafeDateTime "2023-12-30T00:00:00.000Z"
        now = Utils.unsafeDateTime "2024-01-02T00:00:00.000Z"
        metadata = Map.fromFoldable
          [ Utils.unsafeMetadata "prelude" [ Tuple "1.0.0" [ "0.15.10" ], Tuple "2.0.0" [ "0.15.10" ] ]
          , Utils.unsafeMetadata "effect" [ Tuple "1.0.0" [ "0.15.10" ] ]
          , withPublishedTime oldTime $ Utils.unsafeMetadata "old-upload" [ Tuple "1.0.0" [ "0.15.10" ] ]
          , Utils.unsafeMetadata "new-package" [ Tuple "1.0.0" [ "0.15.10" ] ]
          ]
        packageSet = mkPackageSet $ Map.fromFoldable
          [ Tuple (Utils.unsafePackageName "prelude") (Utils.unsafeVersion "1.0.0")
          , Tuple (Utils.unsafePackageName "effect") (Utils.unsafeVersion "1.0.0")
          ]
        expected = Map.fromFoldable
          [ Tuple (Utils.unsafePackageName "prelude") (Utils.unsafeVersion "2.0.0")
          , Tuple (Utils.unsafePackageName "new-package") (Utils.unsafeVersion "1.0.0")
          ]

      PackageSetUpdater.selectPackageSetCandidates (PackageSetUpdater.RecentUploads now (Hours 24.0)) packageSet metadata
        `Assert.shouldEqual` expected

mkPackageSet :: Map PackageName Version -> PackageSet
mkPackageSet packages = PackageSet.PackageSet
  { version: Utils.unsafeVersion "1.0.0"
  , compiler: Utils.unsafeVersion "0.15.10"
  , published: Utils.unsafeDate "2024-01-01"
  , packages
  }

withPublishedTime :: forall a. DateTime -> Tuple a Metadata -> Tuple a Metadata
withPublishedTime publishedTime (Tuple name (Metadata.Metadata metadata)) =
  Tuple name $ Metadata.Metadata $ metadata
    { published = map (_ { publishedTime = publishedTime }) metadata.published }
