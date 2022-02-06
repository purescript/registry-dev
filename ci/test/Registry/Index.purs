module Test.Registry.Index (TestIndexEnv, mkTestIndexEnv, spec) where

import Registry.Prelude

import Control.Monad.Reader as Reader
import Data.Array as Array
import Data.Foldable (sequence_)
import Data.Map as Map
import Data.Set as Set
import Effect.Ref as Ref
import Foreign.FastGlob (Include(..))
import Foreign.FastGlob as FastGlob
import Foreign.Tmp as Tmp
import Node.Path as Node.Path
import Registry.Index (RegistryIndex)
import Registry.Index as Index
import Registry.PackageGraph as PackageGraph
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..))
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Support.Manifest as Support.Manifest

type TestIndexEnv =
  { tmp :: FilePath
  , indexRef :: Ref RegistryIndex
  }

mkTestIndexEnv :: Aff TestIndexEnv
mkTestIndexEnv = liftEffect do
  tmp <- Tmp.mkTmpDir
  indexRef <- Ref.new (Map.empty :: RegistryIndex)
  pure { tmp, indexRef }

spec :: TestIndexEnv -> Spec.Spec Unit
spec env = Spec.hoistSpec identity (\_ m -> Reader.runReaderT m env) testRegistryIndex

testRegistryIndex :: Spec.SpecT (Reader.ReaderT TestIndexEnv Aff) Unit Identity Unit
testRegistryIndex = Spec.before runBefore do
  Spec.describe "Registry index writes to and reads back from disk" do
    Spec.it "Reads an empty index from disk" \{ tmp } -> do
      initialIndex <- lift $ Index.readRegistryIndex tmp
      Map.size initialIndex `Assert.shouldEqual` 0

    sequence_
      [ insertAndCheck Support.Manifest.ab.name Support.Manifest.ab.v1a
      , insertAndCheck Support.Manifest.ab.name Support.Manifest.ab.v1b
      , insertAndCheck Support.Manifest.ab.name Support.Manifest.ab.v2
      , insertAndCheck Support.Manifest.abc.name Support.Manifest.abc.v1
      , insertAndCheck Support.Manifest.abc.name Support.Manifest.abc.v2
      , insertAndCheck Support.Manifest.abcd.name Support.Manifest.abcd.v1
      , insertAndCheck Support.Manifest.abcd.name Support.Manifest.abcd.v2
      ]

    Spec.it "Final on-disk registry index matches the expected directory structure" \{ tmp } -> do
      packagePaths <- liftAff $ FastGlob.match' [ "**/*" ] { cwd: Just tmp, include: FilesOnly }

      let
        expectedPaths = map Node.Path.concat
          [ [ "2", "ab" ]
          , [ "3", "a", "abc" ]
          , [ "ab", "cd", "abcd" ]
          ]

      packagePaths `Assert.shouldEqual` expectedPaths
  where
  runBefore = do
    { tmp, indexRef } <- Reader.ask
    let
      writeMemory = liftEffect <<< flip Ref.write indexRef
    index <- liftEffect $ Ref.read indexRef
    pure { tmp, index, writeMemory }

  insertAndCheck packageName manifest@(Manifest { version }) = do
    let specName = "Inserts " <> PackageName.print packageName <> " version " <> show version

    Spec.it specName \{ tmp, index, writeMemory } -> do
      -- First, we insert the manifest to disk and then read back the result.
      lift $ Index.insertManifest tmp manifest
      diskIndex <- lift $ Index.readRegistryIndex tmp

      let
        -- Then we insert the package into the in-memory index, preferring later
        -- entries in the case of version collisions (as the regsitry index
        -- itself does).
        memoryIndex = Map.insertWith (flip Map.union) packageName (Map.singleton version manifest) index

      _ <- writeMemory memoryIndex

      let
        memoryGraph = PackageGraph.checkRegistryIndex memoryIndex
        sorted = PackageGraph.topologicalSort memoryIndex

      -- Finally, we verify that the on-disk index equals the in-memory index.
      (diskIndex == memoryIndex) `Assert.shouldEqual` true
      (Array.null memoryGraph.unsatisfied) `Assert.shouldEqual` true
      (isSorted sorted) `Assert.shouldEqual` true

-- | Verify that manifests are topographically sorted by their dependencies
isSorted :: Array Manifest -> Boolean
isSorted = fst <<< Array.foldl foldFn (Tuple true Set.empty)
  where
  getDeps :: Manifest -> Array PackageName
  getDeps = Set.toUnfoldable <<< Map.keys <<< _.dependencies <<< un Manifest

  foldFn :: Tuple Boolean (Set PackageName) -> Manifest -> Tuple Boolean (Set PackageName)
  foldFn (Tuple valid visited) manifest@(Manifest { name }) = do
    let newSet = Set.insert name visited
    if all (flip Set.member visited) (getDeps manifest) then
      Tuple (valid && true) newSet
    else
      Tuple false newSet
