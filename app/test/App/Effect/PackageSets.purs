module Test.Registry.App.Effect.PackageSets (spec) where

import Registry.App.Prelude

import Data.Map as Map
import Data.Set as Set
import Effect.Aff as Aff
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Registry.App.Effect.Log (Log(..))
import Registry.App.Effect.Log as Log
import Registry.App.Effect.PackageSets (Change(..))
import Registry.App.Effect.PackageSets as PackageSets
import Registry.App.Effect.Registry (RegistryRead(..))
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage as Storage
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.Tmp as Tmp
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Test.Assert as Assert
import Registry.Test.Utils as Utils
import Registry.Version as Version
import Run as Run
import Run.Except as Except
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "package set recovery" do
    for_ [ true, false ] \hasBackup ->
      Spec.it (if hasBackup then "Discards output after an interrupted upgrade" else "Retains output when no backup remains") do
        Aff.bracket Tmp.mkTmpDir FS.Extra.remove \tmp -> do
          let
            output = Path.concat [ tmp, "output" ]
            backup = Path.concat [ tmp, "output-backup" ]
            oldSet = (un PackageSet packageSet) { packages = Map.singleton (Utils.unsafePackageName "missing") (Utils.unsafeVersion "1.0.0") }
          FS.Extra.ensureDirectory output
          when hasBackup $ FS.Extra.ensureDirectory backup
          -- Stop at installation, after recovery but before invoking a compiler.
          result <- PackageSets.upgradeAtomic (PackageSet oldSet) oldSet.compiler Map.empty
            # PackageSets.interpret (PackageSets.handle { workdir: tmp })
            # Registry.interpretRead
                ( case _ of
                    ReadMetadata _ reply -> pure $ reply $ Right Nothing
                    _ -> Except.throw "Unexpected registry operation"
                )
            # Storage.interpret (\_ -> Except.throw "Unexpected storage operation")
            # Log.interpret (\(Log _ _ next) -> pure next)
            # Except.runExcept
            # Run.runBaseAff'
          result `Assert.shouldEqual` Left "No metadata found for missing@1.0.0"
          liftEffect (FS.Sync.exists output) >>= (_ `Assert.shouldEqual` not hasBackup)
          liftEffect (FS.Sync.exists backup) >>= (_ `Assert.shouldEqual` false)

  Spec.it "Reports package set changelog correctly" do
    let
      operations = Map.fromFoldable
        [ map (const Remove) assert
        , map (Version.bumpPatch >>> Update) effect
        , Tuple (Utils.unsafePackageName "math") (Update (Utils.unsafeVersion "1.0.0"))
        ]

    PackageSets.commitMessage packageSet operations (Utils.unsafeVersion "2.0.0") `Assert.shouldEqual` packageSetCommitMessage

  Spec.it "Reports package set changelog correctly (no updates)" do
    let
      operations = Map.fromFoldable
        [ map (const Remove) assert
        , Tuple (Utils.unsafePackageName "math") (Update (Utils.unsafeVersion "1.0.0"))
        ]

    PackageSets.commitMessage packageSet operations (Utils.unsafeVersion "2.0.0") `Assert.shouldEqual` packageSetCommitMessageNoUpdates

  Spec.describe "orderChanges" do
    Spec.it "Orders removals in reverse topological order (dependents before dependencies)" do
      -- Given: foo depends on bar (bar is a dependency of foo)
      -- When: both are being removed
      -- Then: foo (dependent) should be removed before bar (dependency)
      let
        foo = Utils.unsafePackageName "foo"
        bar = Utils.unsafePackageName "bar"
        v1 = Utils.unsafeVersion "1.0.0"

        index = unsafeFromRight $ ManifestIndex.fromSet ManifestIndex.IgnoreRanges $ Set.fromFoldable
          [ mkManifest bar v1 []
          , mkManifest foo v1 [ bar ]
          ]

        packages = Map.fromFoldable
          [ Tuple bar v1
          , Tuple foo v1
          ]

        changes = Map.fromFoldable
          [ Tuple bar Remove
          , Tuple foo Remove
          ]

        result = PackageSets.orderChanges index packages changes
        names = map fst result

      -- foo (dependent) must come before bar (dependency)
      names `Assert.shouldEqual` [ foo, bar ]

    Spec.it "Orders updates in topological order (dependencies before dependents)" do
      -- Given: foo depends on bar (bar is a dependency of foo)
      -- When: both are being updated
      -- Then: bar (dependency) should be updated before foo (dependent)
      let
        foo = Utils.unsafePackageName "foo"
        bar = Utils.unsafePackageName "bar"
        v2 = Utils.unsafeVersion "2.0.0"

        index = unsafeFromRight $ ManifestIndex.fromSet ManifestIndex.IgnoreRanges $ Set.fromFoldable
          [ mkManifest bar v2 []
          , mkManifest foo v2 [ bar ]
          ]

        packages = Map.fromFoldable
          [ Tuple bar (Utils.unsafeVersion "1.0.0")
          , Tuple foo (Utils.unsafeVersion "1.0.0")
          ]

        changes = Map.fromFoldable
          [ Tuple bar (Update v2)
          , Tuple foo (Update v2)
          ]

        result = PackageSets.orderChanges index packages changes
        names = map fst result

      -- bar (dependency) must come before foo (dependent)
      names `Assert.shouldEqual` [ bar, foo ]

  Spec.describe "extraneousPackageDirs" do
    Spec.it "Returns empty when packages/ matches the target set" do
      let
        target = Map.fromFoldable
          [ Tuple (Utils.unsafePackageName "aff") (Utils.unsafeVersion "7.0.0")
          , Tuple (Utils.unsafePackageName "prelude") (Utils.unsafeVersion "6.0.0")
          ]
        existing = [ "aff@7.0.0", "prelude@6.0.0" ]
      PackageSets.extraneousPackageDirs target existing `Assert.shouldEqual` []

    Spec.it "Identifies stale packages not in the target set" do
      let
        target = Map.fromFoldable
          [ Tuple (Utils.unsafePackageName "aff") (Utils.unsafeVersion "7.0.0")
          ]
        existing = [ "aff@7.0.0", "stale@1.0.0", "old-pkg@2.0.0" ]
      PackageSets.extraneousPackageDirs target existing `Assert.shouldEqual` [ "stale@1.0.0", "old-pkg@2.0.0" ]

    Spec.it "Identifies old versions of updated packages as extraneous" do
      let
        target = Map.fromFoldable
          [ Tuple (Utils.unsafePackageName "aff") (Utils.unsafeVersion "8.0.0")
          ]
        existing = [ "aff@7.0.0" ]
      PackageSets.extraneousPackageDirs target existing `Assert.shouldEqual` [ "aff@7.0.0" ]

    Spec.it "Returns all dirs when target set is empty" do
      let
        target = Map.empty :: Map PackageName Version
        existing = [ "aff@7.0.0", "prelude@6.0.0" ]
      PackageSets.extraneousPackageDirs target existing `Assert.shouldEqual` [ "aff@7.0.0", "prelude@6.0.0" ]

    Spec.it "Returns empty when packages/ is empty" do
      let
        target = Map.fromFoldable
          [ Tuple (Utils.unsafePackageName "aff") (Utils.unsafeVersion "7.0.0")
          ]
      PackageSets.extraneousPackageDirs target [] `Assert.shouldEqual` []

    Spec.it "Processes updates before removals" do
      let
        foo = Utils.unsafePackageName "foo"
        bar = Utils.unsafePackageName "bar"
        v1 = Utils.unsafeVersion "1.0.0"
        v2 = Utils.unsafeVersion "2.0.0"

        index = unsafeFromRight $ ManifestIndex.fromSet ManifestIndex.IgnoreRanges $ Set.fromFoldable
          [ mkManifest bar v1 []
          , mkManifest foo v2 []
          ]

        packages = Map.fromFoldable
          [ Tuple bar v1
          , Tuple foo (Utils.unsafeVersion "1.0.0")
          ]

        changes = Map.fromFoldable
          [ Tuple bar Remove
          , Tuple foo (Update v2)
          ]

        result = PackageSets.orderChanges index packages changes
        names = map fst result

      -- Updates come before removals
      names `Assert.shouldEqual` [ foo, bar ]

packageSet :: PackageSet
packageSet = PackageSet
  { compiler: Utils.unsafeVersion "0.15.2"
  , published: Utils.unsafeDate "2022-07-25"
  , version: Utils.unsafeVersion "4.2.0"
  , packages: Map.fromFoldable do
      [ assert
      , console
      , effect
      , prelude
      ]
  }

assert :: Tuple PackageName Version
assert = Tuple (Utils.unsafePackageName "assert") (Utils.unsafeVersion "6.0.0")

console :: Tuple PackageName Version
console = Tuple (Utils.unsafePackageName "console") (Utils.unsafeVersion "6.0.0")

effect :: Tuple PackageName Version
effect = Tuple (Utils.unsafePackageName "effect") (Utils.unsafeVersion "4.0.0")

prelude :: Tuple PackageName Version
prelude = Tuple (Utils.unsafePackageName "prelude") (Utils.unsafeVersion "6.0.0")

packageSetCommitMessage :: String
packageSetCommitMessage =
  """Release 2.0.0 package set.

New packages:
  - math@1.0.0

Updated packages:
  - effect@4.0.0 -> 4.0.1

Removed packages:
  - assert@6.0.0
"""

packageSetCommitMessageNoUpdates :: String
packageSetCommitMessageNoUpdates =
  """Release 2.0.0 package set.

New packages:
  - math@1.0.0

Removed packages:
  - assert@6.0.0
"""

mkManifest :: PackageName -> Version -> Array PackageName -> Manifest
mkManifest name version deps = do
  let toRange v = ">=" <> Version.print v <> " <" <> Version.print (Version.bumpHighest v)
  Utils.unsafeManifest
    (PackageName.print name)
    (Version.print version)
    (map (\dep -> Tuple (PackageName.print dep) (toRange (Utils.unsafeVersion "1.0.0"))) deps)
