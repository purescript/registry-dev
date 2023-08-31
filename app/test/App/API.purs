module Test.Registry.App.API (spec) where

import Registry.App.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Set as Set
import Data.String.NonEmpty as NonEmptyString
import Effect.Aff as Aff
import Effect.Ref as Ref
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.API as API
import Registry.App.Effect.Env as Env
import Registry.App.Effect.Log as Log
import Registry.App.Effect.Pursuit as Pursuit
import Registry.App.Effect.Registry as Registry
import Registry.App.Effect.Storage as Storage
import Registry.App.Legacy.Types (RawPackageName(..))
import Registry.Constants as Constants
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Tmp as Tmp
import Registry.PackageName as PackageName
import Registry.Test.Assert as Assert
import Registry.Test.Assert.Run as Assert.Run
import Registry.Test.Utils as Utils
import Registry.Version as Version
import Run (EFFECT, Run)
import Run as Run
import Run.Except as Except
import Test.Spec as Spec

-- | The environment accessible to each assertion in the test suite, derived
-- | from the fixtures.
type PipelineEnv =
  { workdir :: FilePath
  , metadata :: Ref (Map PackageName Metadata)
  , index :: Ref ManifestIndex
  , storageDir :: FilePath
  , githubDir :: FilePath
  }

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Verifies build plans" do
    checkBuildPlanToResolutions

  Spec.describe "Includes correct files in tarballs" do
    removeIgnoredTarballFiles
    copySourceFiles

  Spec.describe "API pipelines run correctly" $ Spec.around withCleanEnv do
    Spec.it "Publish" \{ workdir, index, metadata, storageDir, githubDir } -> do
      let testEnv = { workdir, index, metadata, username: "jon", storage: storageDir, github: githubDir }
      Assert.Run.runTestEffects testEnv do
        -- We'll publish effect@4.0.0
        let
          name = Utils.unsafePackageName "effect"
          version = Utils.unsafeVersion "4.0.0"
          ref = "v4.0.0"
          publishArgs =
            { compiler: Utils.unsafeVersion "0.15.9"
            , location: Just $ GitHub { owner: "purescript", repo: "purescript-effect", subdir: Nothing }
            , name
            , ref
            , resolutions: Nothing
            }

        -- First, we publish the package.
        API.publish CurrentPackage publishArgs

        -- Then, we can check that it did make it to "Pursuit" as expected
        Pursuit.getPublishedVersions name >>= case _ of
          Right versions | isJust (Map.lookup version versions) -> pure unit
          Right _ -> Except.throw $ "Expected " <> formatPackageVersion name version <> " to be published to Pursuit."
          Left err -> Except.throw $ "Failed to get published versions: " <> err

        -- As well as to the storage backend
        Storage.query name >>= \versions ->
          unless (Set.member version versions) do
            Except.throw $ "Expected " <> formatPackageVersion name version <> " to be published to registry storage."

        -- Finally, we can verify that publishing the package again should fail
        -- since it already exists.
        Except.runExcept (API.publish CurrentPackage publishArgs) >>= case _ of
          Left _ -> pure unit
          Right _ -> Except.throw $ "Expected publishing " <> formatPackageVersion name version <> " twice to fail."
  where
  withCleanEnv :: (PipelineEnv -> Aff Unit) -> Aff Unit
  withCleanEnv action = do
    cwd <- liftEffect Process.cwd
    workdir <- liftAff Tmp.mkTmpDir
    Aff.bracket (enterCleanEnv workdir) (exitCleanEnv cwd) action
    where
    -- Exits the clean environment for the test
    exitCleanEnv :: FilePath -> PipelineEnv -> Aff Unit
    exitCleanEnv cwd { workdir } = do
      liftEffect $ Process.chdir cwd
      FS.Extra.remove workdir

    -- Sets up a clean environment for each test, beginning with only what's in
    -- the fixtures directory.
    enterCleanEnv :: FilePath -> Aff PipelineEnv
    enterCleanEnv workdir = do
      Env.loadEnvFile (Path.concat [ "..", ".env.example" ])

      -- FIXME: The publish pipeline probably shouldn't require this. But...the
      -- publish pipeline requires that there be a 'types' directory containing
      -- dhall types for the registry in the current working directory.
      FS.Extra.copy { from: Path.concat [ "..", "types" ], to: Path.concat [ workdir, "types" ], preserveTimestamps: true }

      testFixtures <- liftAff Tmp.mkTmpDir
      let copyFixture path = FS.Extra.copy { from: Path.concat [ "fixtures", path ], to: Path.concat [ testFixtures, path ], preserveTimestamps: true }

      -- Set up a clean fixtures environment.
      liftAff do
        copyFixture "registry-index"
        copyFixture "registry"
        copyFixture "registry-storage"
        copyFixture "github-packages"

      let
        readFixtures = do
          initialMetadata <- Registry.readAllMetadataFromDisk $ Path.concat [ testFixtures, "registry", "metadata" ]
          metadata <- liftEffect $ Ref.new initialMetadata
          initialIndex <- Registry.readManifestIndexFromDisk $ Path.concat [ testFixtures, "registry-index" ]
          index <- liftEffect $ Ref.new initialIndex
          pure { metadata, index }

      fixtures <- readFixtures
        # Log.interpret (\(Log.Log _ _ next) -> pure next)
        # Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
        # Run.runBaseAff'

      liftEffect $ Process.chdir workdir
      pure
        { workdir
        , metadata: fixtures.metadata
        , index: fixtures.index
        , storageDir: Path.concat [ testFixtures, "registry-storage" ]
        , githubDir: Path.concat [ testFixtures, "github-packages" ]
        }

checkBuildPlanToResolutions :: Spec.Spec Unit
checkBuildPlanToResolutions = do
  Spec.it "buildPlanToResolutions produces expected resolutions file format" do
    Assert.shouldEqual generatedResolutions expectedResolutions
  where
  dependenciesDir = "testDir"

  resolutions = Map.fromFoldable
    [ Tuple (Utils.unsafePackageName "prelude") (Utils.unsafeVersion "1.0.0")
    , Tuple (Utils.unsafePackageName "bifunctors") (Utils.unsafeVersion "2.0.0")
    , Tuple (Utils.unsafePackageName "ordered-collections") (Utils.unsafeVersion "3.0.0")
    ]

  generatedResolutions =
    API.formatPursuitResolutions
      { resolutions
      , dependenciesDir
      }

  expectedResolutions = Map.fromFoldable do
    packageName /\ version <- (Map.toUnfoldable resolutions :: Array _)
    let
      bowerName = RawPackageName ("purescript-" <> PackageName.print packageName)
      path = Path.concat [ dependenciesDir, PackageName.print packageName <> "-" <> Version.print version ]
    pure $ Tuple bowerName { path, version }

removeIgnoredTarballFiles :: Spec.Spec Unit
removeIgnoredTarballFiles = Spec.before runBefore do
  Spec.it "Picks correct files when packaging a tarball" \{ tmp, writeDirectories, writeFiles } -> do
    let
      goodDirectories = [ "src" ]
      goodFiles = [ "purs.json", "README.md", "LICENSE", Path.concat [ "src", "Main.purs" ], Path.concat [ "src", "Main.js" ] ]

    writeDirectories (goodDirectories <> Constants.ignoredDirectories)
    writeFiles (goodFiles <> Constants.ignoredFiles)

    API.removeIgnoredTarballFiles tmp

    paths <- FastGlob.match tmp [ "**/*" ]

    let
      ignoredPaths = Constants.ignoredDirectories <> Constants.ignoredFiles
      acceptedPaths = goodDirectories <> goodFiles

    for_ ignoredPaths \path ->
      paths.succeeded `Assert.shouldNotContain` path

    for_ acceptedPaths \path -> do
      paths.succeeded `Assert.shouldContain` path
  where
  runBefore = do
    tmp <- Tmp.mkTmpDir

    let
      inTmp :: FilePath -> FilePath
      inTmp path = Path.concat [ tmp, path ]

      writeDirectories :: Array FilePath -> _
      writeDirectories = traverse_ (FS.Extra.ensureDirectory <<< inTmp)

      writeFiles :: Array FilePath -> _
      writeFiles = traverse_ (\path -> FS.Aff.writeTextFile UTF8 (inTmp path) "<test>")

    pure { tmp, writeDirectories, writeFiles }

copySourceFiles :: Spec.Spec Unit
copySourceFiles = Spec.hoistSpec identity (\_ -> Assert.Run.runBaseEffects) $ Spec.before runBefore do
  let
    goodDirectories = [ "src" ]
    goodFiles = [ "purs.json", "README.md", "LICENSE", Path.concat [ "src", "Main.purs" ], Path.concat [ "src", "Main.js" ] ]

  Spec.it "Only copies always-included files by default" \{ source, destination, writeDirectories, writeFiles } -> do
    writeDirectories (goodDirectories <> Constants.ignoredDirectories <> [ "test" ])
    writeFiles (goodFiles <> Constants.ignoredFiles <> [ Path.concat [ "test", "Main.purs" ] ])

    API.copyPackageSourceFiles { includeFiles: Nothing, excludeFiles: Nothing, source, destination }

    paths <- FastGlob.match destination [ "**/*" ]

    let
      acceptedPaths = goodDirectories <> goodFiles
      ignoredPaths = Constants.ignoredDirectories <> Constants.ignoredFiles

    for_ acceptedPaths \path -> do
      paths.succeeded `Assert.Run.shouldContain` path

    for_ ignoredPaths \path -> do
      paths.succeeded `Assert.Run.shouldNotContain` path

  Spec.it "Copies user-specified files" \{ source, destination, writeDirectories, writeFiles } -> do
    let
      includeFiles = NonEmptyArray.fromArray =<< sequence [ NonEmptyString.fromString "test/**/*.purs" ]
      testDir = [ "test" ]
      testFiles = [ Path.concat [ "test", "Main.purs" ], Path.concat [ "test", "Test.purs" ] ]

    writeDirectories (goodDirectories <> testDir)
    writeFiles (goodFiles <> testFiles)

    API.copyPackageSourceFiles { includeFiles, excludeFiles: Nothing, source, destination }

    paths <- FastGlob.match destination [ "**/*" ]

    let acceptedPaths = goodDirectories <> goodFiles <> testDir <> testFiles

    for_ acceptedPaths \path -> do
      paths.succeeded `Assert.Run.shouldContain` path

  Spec.it "Does not copy user-specified excluded files" \{ source, destination, writeDirectories, writeFiles } -> do
    let
      includeFiles = NonEmptyArray.fromArray =<< sequence [ NonEmptyString.fromString "test/**/*.purs" ]
      excludeFiles = NonEmptyArray.fromArray =<< sequence [ NonEmptyString.fromString "test/**/Test.purs" ]
      testDir = [ "test" ]
      testMain = Path.concat [ "test", "Main.purs" ]
      testTest = Path.concat [ "test", "Test.purs" ]
      testFiles = [ testMain, testTest ]

    writeDirectories (goodDirectories <> testDir)
    writeFiles (goodFiles <> testFiles)

    API.copyPackageSourceFiles { includeFiles, excludeFiles, source, destination }

    paths <- FastGlob.match destination [ "**/*" ]

    let acceptedPaths = goodDirectories <> goodFiles <> testDir <> [ testMain ]

    for_ acceptedPaths \path -> do
      paths.succeeded `Assert.Run.shouldContain` path

  Spec.it "Won't exclude always included files" \{ source, destination, writeDirectories, writeFiles } -> do
    let
      includeFiles = NonEmptyArray.fromArray =<< sequence [ NonEmptyString.fromString "test/**/*.purs" ]
      excludeFiles = Just $ NonEmptyArray.singleton (NonEmptyString.nes (Proxy :: _ "purs.json"))

    writeDirectories (goodDirectories)
    writeFiles (goodFiles)

    API.copyPackageSourceFiles { includeFiles, excludeFiles, source, destination }

    paths <- FastGlob.match destination [ "**/*" ]

    let acceptedPaths = goodDirectories <> goodFiles

    for_ acceptedPaths \path -> do
      paths.succeeded `Assert.Run.shouldContain` path

  where
  runBefore :: forall r. Run (EFFECT + r) _
  runBefore = do
    tmp <- Tmp.mkTmpDir
    destTmp <- Tmp.mkTmpDir

    let
      inTmp :: FilePath -> FilePath
      inTmp path = Path.concat [ tmp, path ]

      writeDirectories :: Array FilePath -> _
      writeDirectories = traverse_ (FS.Extra.ensureDirectory <<< inTmp)

      writeFiles :: Array FilePath -> _
      writeFiles = Run.liftAff <<< traverse_ (\path -> FS.Aff.writeTextFile UTF8 (inTmp path) "module Module where")

    pure { source: tmp, destination: destTmp, writeDirectories, writeFiles }
