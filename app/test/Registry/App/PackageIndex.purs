module Test.Registry.App.PackageIndex (TestIndexEnv, mkTestIndexEnv, spec) where

import Registry.App.Prelude

import Control.Monad.Reader as Reader
import Data.Argonaut.Core as Argonaut
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Foldable (sequence_)
import Data.Map as Map
import Data.Set as Set
import Effect.Ref as Ref
import Foreign.FastGlob (Include(..))
import Foreign.FastGlob as FastGlob
import Foreign.Node.FS as FS.Extra
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.PackageIndex as PackageIndex
import Registry.Internal.Codec as Internal.Codec
import Registry.License as License
import Registry.Location (Location(..))
import Registry.Manifest (Manifest(..))
import Registry.Manifest as Manifest
import Registry.ManifestIndex (ManifestIndex)
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.RegistryM (RegistryM)
import Registry.RegistryM as RegistryM
import Registry.Version as Version
import Test.Assert as Assert
import Test.Fixture.Manifest as Fixture
import Test.RegistrySpec as RegistrySpec
import Test.Spec as Spec

type TestIndexEnv =
  { tmp :: FilePath
  , indexRef :: Ref ManifestIndex
  }

mkTestIndexEnv :: Aff TestIndexEnv
mkTestIndexEnv = liftEffect do
  tmp <- Tmp.mkTmpDir
  indexRef <- Ref.new ManifestIndex.empty
  pure { tmp, indexRef }

spec :: TestIndexEnv -> Spec.Spec Unit
spec env = Spec.hoistSpec identity (\_ m -> Reader.runReaderT m env) testRegistryIndex

runTestRegistryM :: forall m a. MonadAff m => FilePath -> RegistryM a -> m a
runTestRegistryM index =
  liftAff <<< RegistryM.runRegistryM (RegistrySpec.defaultTestEnv { registryIndex = index })

testRegistryIndex :: Spec.SpecT (Reader.ReaderT TestIndexEnv Aff) Unit Identity Unit
testRegistryIndex = Spec.before runBefore do
  Spec.describe "Registry index writes to and reads back from disk" do
    Spec.it "Reads an empty index from disk" \{ tmp } -> do
      initialIndex <- runTestRegistryM tmp PackageIndex.readManifestIndexFromDisk
      Map.size (ManifestIndex.toMap initialIndex) `Assert.shouldEqual` 0

    let
      mkManifest :: String -> String -> Maybe String -> Manifest
      mkManifest name version description =
        Fixture.fixture
          # Fixture.setName name
          # Fixture.setVersion version
          # maybe identity Fixture.setDescription description

    sequence_
      [ insertAndCheck $ mkManifest "ab" "1.0.0" Nothing
      , insertAndCheck $ mkManifest "ab" "1.0.0" (Just "Description")
      , insertAndCheck $ mkManifest "ab" "2.0.0" Nothing
      , insertAndCheck $ mkManifest "abc" "1.0.0" Nothing
      , insertAndCheck $ mkManifest "abc" "2.0.0" Nothing
      , insertAndCheck $ mkManifest "abcd" "1.0.0" Nothing
      , insertAndCheck $ mkManifest "abcd" "2.0.0" Nothing
      ]

    Spec.it "Final on-disk registry index matches the expected directory structure" \{ tmp } -> do
      packagePaths <- liftAff $ FastGlob.match' tmp [ "**/*" ] { include: FilesOnly }

      let
        expectedPaths = map Path.concat
          [ [ "2", "ab" ]
          , [ "3", "a", "abc" ]
          , [ "ab", "cd", "abcd" ]
          ]

      Array.sort packagePaths.succeeded `Assert.shouldEqual` expectedPaths

    Spec.it "Inserts into real-world manifest example" \_ -> do
      tmp <- liftEffect Tmp.mkTmpDir
      liftAff do
        -- This was an issue in the real-world registry index, in which a file
        -- with the first three context versions would be overwritten when the
        -- fourth and final version was inserted. This was due to the file being
        -- written with a trailing newline, which caused it to fail to parse.
        let
          contextName = unsafeFromRight $ PackageName.parse "context"
          contextDir = Path.concat [ tmp, ManifestIndex.packageEntryDirectory contextName ]
          contextPath = Path.concat [ tmp, ManifestIndex.packageEntryFilePath contextName ]

        FS.Extra.ensureDirectory contextDir
        FS.Aff.writeTextFile ASCII contextPath contextFile

        result <- ManifestIndex.insertIntoEntryFile tmp $ contextManifest "1.0.0"
        case result of
          Left error -> Assert.fail error
          Right _ -> pure unit
        index <- runTestRegistryM tmp PackageIndex.readManifestIndexFromDisk
        (Map.size <$> Map.lookup contextName (ManifestIndex.toMap index)) `Assert.shouldEqual` Just 4
  where
  runBefore = do
    { tmp, indexRef } <- Reader.ask
    let writeMemory = liftEffect <<< flip Ref.write indexRef
    index <- liftEffect $ Ref.read indexRef
    pure { tmp, index, writeMemory }

  insertAndCheck manifest@(Manifest { name: packageName, version }) = do
    let specName = "Inserts " <> PackageName.print packageName <> " version " <> Version.print version

    Spec.it specName \{ tmp, index, writeMemory } -> do
      -- First, we insert the manifest to disk and then read back the result.
      result <- ManifestIndex.insertIntoEntryFile tmp manifest
      case result of
        Left error -> Assert.fail $ "Failed to write to file " <> ManifestIndex.packageEntryFilePath packageName <> "\n" <> error
        Right _ -> pure unit
      diskIndex <- runTestRegistryM tmp PackageIndex.readManifestIndexFromDisk

      case ManifestIndex.insert manifest index of
        Left errors ->
          Assert.fail
            $ append ("Failed to insert " <> PackageName.print packageName <> "\n")
            $ Argonaut.stringifyWithIndent 2
            $ CA.encode (Internal.Codec.packageMap Range.codec) errors
        Right memoryIndex -> do
          _ <- writeMemory memoryIndex
          let sortedMemoryIndex = ManifestIndex.toSortedArray memoryIndex
          let sortedDiskIndex = ManifestIndex.toSortedArray diskIndex
          let print = Argonaut.stringify <<< CA.encode (CA.array Manifest.codec)
          print sortedDiskIndex `Assert.shouldEqual` print sortedMemoryIndex
          isSorted sortedMemoryIndex `Assert.shouldEqual` true

-- | Verify that manifests are topologically sorted by their dependencies
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

contextManifest :: String -> Manifest
contextManifest version =
  Manifest
    { name: unsafeFromRight $ PackageName.parse "context"
    , owners: Nothing
    , version: unsafeFromRight $ Version.parse version
    , license: unsafeFromRight $ License.parse "MIT"
    , location: GitHub { owner: "Fresheyeball", repo: "purescript-owner", subdir: Nothing }
    , description: Nothing
    , files: Nothing
    , dependencies: Map.empty
    }

contextFile :: String
contextFile =
  """
{"name":"context","version":"0.0.1","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
{"name":"context","version":"0.0.2","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
{"name":"context","version":"0.0.3","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
"""
