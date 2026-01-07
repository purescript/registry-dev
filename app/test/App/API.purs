module Test.Registry.App.API (spec) where

import Registry.App.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.API as API
import Registry.App.CLI.Tar as Tar
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
import Registry.Internal.Codec as Internal.Codec
import Registry.Manifest as Manifest
import Registry.ManifestIndex as ManifestIndex
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Solver as Solver
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
  , archiveDir :: FilePath
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
    Spec.it "Publish a legacy-converted package with unused deps" \{ workdir, index, metadata, storageDir, archiveDir, githubDir } -> do
      logs <- liftEffect (Ref.new [])

      let
        toLegacyIndex :: ManifestIndex -> Solver.TransitivizedRegistry
        toLegacyIndex =
          Solver.exploreAllTransitiveDependencies
            <<< Solver.initializeRegistry
            <<< map (map (_.dependencies <<< un Manifest))
            <<< ManifestIndex.toMap

        testEnv =
          { workdir
          , logs
          , index
          , metadata
          , pursuitExcludes: Set.singleton (Utils.unsafePackageName "type-equality")
          , username: "jon"
          , storage: storageDir
          , archive: archiveDir
          , github: githubDir
          }

      result <- Assert.Run.runTestEffects testEnv $ Except.runExcept do
        -- We'll publish effect@4.0.0 from the fixtures/github-packages
        -- directory, which has an unnecessary dependency on 'type-equality'
        -- inserted into it.
        let
          name = Utils.unsafePackageName "effect"
          version = Utils.unsafeVersion "4.0.0"
          ref = "v4.0.0"
          publishArgs =
            { compiler: Utils.unsafeVersion "0.15.9"
            , location: Just $ GitHub { owner: "purescript", repo: "purescript-effect", subdir: Nothing }
            , name
            , ref
            , version: version
            , resolutions: Nothing
            }

        -- First, we publish the package.
        Registry.readAllManifests >>= \idx ->
          void $ API.publish (Just (toLegacyIndex idx)) publishArgs

        -- Then, we can check that it did make it to "Pursuit" as expected
        Pursuit.getPublishedVersions name >>= case _ of
          Right versions | isJust (Map.lookup version versions) -> pure unit
          Right _ -> Except.throw $ "Expected " <> formatPackageVersion name version <> " to be published to Pursuit."
          Left err -> Except.throw $ "Failed to get published versions: " <> err

        -- As well as to the storage backend
        Storage.query name >>= \versions ->
          unless (Set.member version versions) do
            Except.throw $ "Expected " <> formatPackageVersion name version <> " to be published to registry storage."

        -- Let's verify the manifest does not include the unnecessary
        -- 'type-equality' dependency...
        Storage.download name version "effect-result"
        Tar.extract { cwd: workdir, archive: "effect-result" }
        Run.liftAff (readJsonFile Manifest.codec (Path.concat [ "effect-4.0.0", "purs.json" ])) >>= case _ of
          Left err -> Except.throw $ "Expected effect@4.0.0 to be downloaded to effect-4.0.0 with a purs.json but received error " <> err
          Right (Manifest manifest) -> do
            let expectedDeps = Map.singleton (Utils.unsafePackageName "prelude") (Utils.unsafeRange ">=6.0.0 <7.0.0")
            when (manifest.dependencies /= expectedDeps) do
              Except.throw $ String.joinWith "\n"
                [ "Expected effect@4.0.0 to have dependencies"
                , printJson (Internal.Codec.packageMap Range.codec) expectedDeps
                , "\nbut got"
                , printJson (Internal.Codec.packageMap Range.codec) manifest.dependencies
                ]

        -- We should verify the resulting metadata file is correct
        Metadata effectMetadata <- Registry.readMetadata name >>= case _ of
          Nothing -> Except.throw $ "Expected " <> PackageName.print name <> " to be in metadata."
          Just m -> pure m

        case Map.lookup version effectMetadata.published of
          Nothing -> Except.throw $ "Expected " <> formatPackageVersion name version <> " to be in metadata."
          Just published -> do
            let many' = NonEmptyArray.toArray published.compilers
            let expected = map Utils.unsafeVersion [ "0.15.9", "0.15.10" ]
            unless (many' == expected) do
              Except.throw $ "Expected " <> formatPackageVersion name version <> " to have a compiler matrix of " <> Utils.unsafeStringify (map Version.print expected) <> " but got " <> Utils.unsafeStringify (map Version.print many')

        -- Finally, we can verify that publishing the package again should fail
        -- since it already exists.
        Except.runExcept (API.publish Nothing publishArgs) >>= case _ of
          Left _ -> pure unit
          Right _ -> Except.throw $ "Expected publishing " <> formatPackageVersion name version <> " twice to fail."

        -- If we got here then the new published package is fine. There is one
        -- other successful code path: publishing a package that already exists
        -- but did not have documentation make it to Pursuit.
        let
          pursuitOnlyPublishArgs =
            { compiler: Utils.unsafeVersion "0.15.9"
            , location: Just $ GitHub { owner: "purescript", repo: "purescript-type-equality", subdir: Nothing }
            , name: Utils.unsafePackageName "type-equality"
            , ref: "v4.0.1"
            , version: Utils.unsafeVersion "4.0.1"
            , resolutions: Nothing
            }
        Registry.readAllManifests >>= \idx ->
          void $ API.publish (Just (toLegacyIndex idx)) pursuitOnlyPublishArgs

        -- We can also verify that transitive dependencies are added for legacy
        -- packages.
        let
          transitive = { name: Utils.unsafePackageName "transitive", version: Utils.unsafeVersion "1.0.0" }
          transitivePublishArgs =
            { compiler: Utils.unsafeVersion "0.15.9"
            , location: Just $ GitHub { owner: "purescript", repo: "purescript-transitive", subdir: Nothing }
            , name: transitive.name
            , ref: "v" <> Version.print transitive.version
            , version: transitive.version
            , resolutions: Nothing
            }
        Registry.readAllManifests >>= \idx ->
          void $ API.publish (Just (toLegacyIndex idx)) transitivePublishArgs

        -- We should verify the resulting metadata file is correct
        Metadata transitiveMetadata <- Registry.readMetadata transitive.name >>= case _ of
          Nothing -> Except.throw $ "Expected " <> PackageName.print transitive.name <> " to be in metadata."
          Just m -> pure m

        case Map.lookup transitive.version transitiveMetadata.published of
          Nothing -> Except.throw $ "Expected " <> formatPackageVersion transitive.name transitive.version <> " to be in metadata."
          Just published -> do
            let many' = NonEmptyArray.toArray published.compilers
            let expected = map Utils.unsafeVersion [ "0.15.9", "0.15.10" ]
            unless (many' == expected) do
              Except.throw $ "Expected " <> formatPackageVersion transitive.name transitive.version <> " to have a compiler matrix of " <> Utils.unsafeStringify (map Version.print expected) <> " but got " <> Utils.unsafeStringify (map Version.print many')

        Registry.readManifest transitive.name transitive.version >>= case _ of
          Nothing -> Except.throw $ "Expected " <> PackageName.print transitive.name <> " to be in manifest index."
          Just (Manifest manifest) -> do
            let expectedDeps = Map.singleton (Utils.unsafePackageName "prelude") (Utils.unsafeRange ">=6.0.0 <7.0.0")
            when (manifest.dependencies /= expectedDeps) do
              Except.throw $ String.joinWith "\n"
                [ "Expected transitive@1.0.0 to have dependencies"
                , printJson (Internal.Codec.packageMap Range.codec) expectedDeps
                , "\nbut got"
                , printJson (Internal.Codec.packageMap Range.codec) manifest.dependencies
                ]

      case result of
        Left exn -> do
          recorded <- liftEffect (Ref.read logs)
          Console.error $ String.joinWith "\n" (map (\(Tuple _ msg) -> msg) recorded)
          Assert.fail $ "Got an Aff exception! " <> Aff.message exn
        Right (Left err) -> do
          recorded <- liftEffect (Ref.read logs)
          Console.error $ String.joinWith "\n" (map (\(Tuple _ msg) -> msg) recorded)
          Assert.fail $ "Expected to publish effect@4.0.0 and type-equality@4.0.1 and transitive@1.0.0 but got error: " <> err
        Right (Right _) -> pure unit

    Spec.it "Falls back to archive when GitHub repo is inaccessible during legacy import" \{ workdir, index, metadata, storageDir, archiveDir, githubDir } -> do
      logs <- liftEffect (Ref.new [])

      let
        toLegacyIndex :: ManifestIndex -> Solver.TransitivizedRegistry
        toLegacyIndex =
          Solver.exploreAllTransitiveDependencies
            <<< Solver.initializeRegistry
            <<< map (map (_.dependencies <<< un Manifest))
            <<< ManifestIndex.toMap

        testEnv =
          { workdir
          , logs
          , index
          , metadata
          , pursuitExcludes: Set.empty
          , username: "jon"
          , storage: storageDir
          , archive: archiveDir
          , github: githubDir
          }

      -- The prelude@6.0.2 package exists in registry-archive but NOT in
      -- github-packages or registry-storage. This simulates an archive-backed
      -- package whose original GitHub repo is gone.
      result <- Assert.Run.runTestEffects testEnv $ Except.runExcept do
        let
          name = Utils.unsafePackageName "prelude"
          version = Utils.unsafeVersion "6.0.2"
          ref = "v6.0.2"
          publishArgs =
            { compiler: Utils.unsafeVersion "0.15.9"
            , location: Just $ GitHub { owner: "purescript", repo: "purescript-prelude", subdir: Nothing }
            , name
            , ref
            , version
            , resolutions: Nothing
            }

        -- Legacy import with archive fallback
        Registry.readAllManifests >>= \idx ->
          void $ API.publish (Just (toLegacyIndex idx)) publishArgs

        -- Verify the package was published to storage
        Storage.query name >>= \versions ->
          unless (Set.member version versions) do
            Except.throw $ "Expected " <> formatPackageVersion name version <> " to be published to registry storage."

      case result of
        Left exn -> do
          recorded <- liftEffect (Ref.read logs)
          Console.error $ String.joinWith "\n" (map (\(Tuple _ msg) -> msg) recorded)
          Assert.fail $ "Got an Aff exception! " <> Aff.message exn
        Right (Left err) -> do
          recorded <- liftEffect (Ref.read logs)
          Console.error $ String.joinWith "\n" (map (\(Tuple _ msg) -> msg) recorded)
          Assert.fail $ "Expected prelude@6.0.2 to be published via archive fallback but got error: " <> err
        Right (Right _) -> pure unit
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
      Env.loadEnvFile ".env.example"

      -- FIXME: The publish pipeline probably shouldn't require this. But...the
      -- publish pipeline requires that there be a 'types' directory containing
      -- dhall types for the registry in the current working directory.
      FS.Extra.copy { from: "types", to: Path.concat [ workdir, "types" ], preserveTimestamps: true }

      testFixtures <- liftAff Tmp.mkTmpDir
      let copyFixture path = FS.Extra.copy { from: Path.concat [ "app", "fixtures", path ], to: Path.concat [ testFixtures, path ], preserveTimestamps: true }

      -- Set up a clean fixtures environment.
      liftAff do
        copyFixture "registry-index"
        copyFixture "registry"
        copyFixture "registry-storage"
        copyFixture "registry-archive"
        copyFixture "github-packages"
        -- FIXME: This is a bit hacky, but we remove effect-4.0.0.tar.gz since the unit test publishes
        -- it from scratch and will fail if effect-4.0.0 is already in storage. We have it in storage
        -- for the separate integration tests.
        FS.Extra.remove $ Path.concat [ testFixtures, "registry-storage", "effect-4.0.0.tar.gz" ]

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
        , archiveDir: Path.concat [ testFixtures, "registry-archive" ]
        , githubDir: Path.concat [ testFixtures, "github-packages" ]
        }

checkBuildPlanToResolutions :: Spec.Spec Unit
checkBuildPlanToResolutions = do
  Spec.it "buildPlanToResolutions produces expected resolutions file format" do
    Assert.shouldEqual generatedResolutions expectedResolutions
  where
  installedResolutions = "testDir"

  resolutions = Map.fromFoldable
    [ Tuple (Utils.unsafePackageName "prelude") (Utils.unsafeVersion "1.0.0")
    , Tuple (Utils.unsafePackageName "bifunctors") (Utils.unsafeVersion "2.0.0")
    , Tuple (Utils.unsafePackageName "ordered-collections") (Utils.unsafeVersion "3.0.0")
    ]

  generatedResolutions =
    API.formatPursuitResolutions
      { resolutions
      , installedResolutions
      }

  expectedResolutions = Map.fromFoldable do
    packageName /\ version <- (Map.toUnfoldable resolutions :: Array _)
    let
      bowerName = RawPackageName ("purescript-" <> PackageName.print packageName)
      path = Path.concat [ installedResolutions, PackageName.print packageName <> "-" <> Version.print version ]
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
