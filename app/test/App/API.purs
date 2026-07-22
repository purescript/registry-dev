module Test.Registry.App.API (spec) where

import Registry.App.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Set as Set
import Data.String as String
import Data.String.NonEmpty as NonEmptyString
import Data.String.Pattern (Pattern(..), Replacement(..))
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.API (LicenseValidationError(..), validateLicense)
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
import Registry.License as License
import Registry.Location (Location(..))
import Registry.Operation (PublishData)
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
-- |
-- | `failurePlan` is shared by the effect mocks and consumed in order as its
-- | matching operations run. This lets a test observe each partial state while
-- | retrying against the same environment without special production code.
type PipelineEnv =
  { workdir :: FilePath
  , failurePlan :: Ref (Array Assert.Run.TestFailure)
  , metadata :: Ref (Map PackageName Metadata)
  , initialMetadata :: Map PackageName Metadata
  , index :: Ref ManifestIndex
  , initialIndex :: ManifestIndex
  , storageDir :: FilePath
  , githubDir :: FilePath
  }

effectName :: PackageName
effectName = Utils.unsafePackageName "effect"

effectVersion :: Version
effectVersion = Utils.unsafeVersion "4.0.0"

effectPublishArgs :: PublishData
effectPublishArgs =
  { compiler: Just $ Utils.unsafeVersion "0.15.10"
  , location: Just $ GitHub { owner: "purescript", repo: "purescript-effect", subdir: Nothing }
  , name: effectName
  , ref: "v4.0.0"
  , version: effectVersion
  , resolutions: Nothing
  }

assertPublicationState
  :: forall r
   . PackageName
  -> Version
  -> { manifest :: Boolean, metadata :: Boolean, storage :: Boolean }
  -> Run (Registry.REGISTRY_READ + Storage.STORAGE + Except.EXCEPT String + r) Unit
assertPublicationState name version expected = do
  storedVersions <- Storage.query name
  maybeMetadata <- Registry.readMetadata name
  maybeManifest <- Registry.readManifest name version
  let
    actualStorage = Set.member version storedVersions
    actualMetadata = case maybeMetadata of
      Nothing -> false
      Just (Metadata { published }) -> Map.member version published
    actualManifest = isJust maybeManifest
  unless (actualStorage == expected.storage) do
    Except.throw $ "Expected storage presence to be " <> show expected.storage <> " but got " <> show actualStorage
  unless (actualMetadata == expected.metadata) do
    Except.throw $ "Expected metadata presence to be " <> show expected.metadata <> " but got " <> show actualMetadata
  unless (actualManifest == expected.manifest) do
    Except.throw $ "Expected manifest presence to be " <> show expected.manifest <> " but got " <> show actualManifest

runPipelineAssertion :: PipelineEnv -> Run Assert.Run.TEST_EFFECTS Unit -> Aff Unit
runPipelineAssertion env action = do
  logs <- liftEffect (Ref.new [])
  result <- Assert.Run.runTestEffects
    { workdir: env.workdir
    , failurePlan: env.failurePlan
    , logs
    , index: env.index
    , metadata: env.metadata
    , pursuitExcludes: Set.empty
    , username: "jon"
    , storage: env.storageDir
    , github: env.githubDir
    }
    action
  case result of
    Left exn -> do
      recorded <- liftEffect (Ref.read logs)
      Console.error $ String.joinWith "\n" (map (\(Tuple _ msg) -> msg) recorded)
      Assert.fail $ "Got an Aff exception! " <> Aff.message exn
    Right _ -> pure unit

resetPublicationState :: forall m. MonadAff m => MonadEffect m => PipelineEnv -> m Unit
resetPublicationState env = do
  liftAff $ FS.Extra.remove $ Path.concat [ env.storageDir, "effect-4.0.0.tar.gz" ]
  liftEffect do
    Ref.write [] env.failurePlan
    Ref.write env.initialMetadata env.metadata
    Ref.write env.initialIndex env.index

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Verifies build plans" do
    checkBuildPlanToResolutions

  Spec.describe "Parses source manifests" do
    parseSourceManifestSpec

  Spec.describe "Validates licenses match" do
    licenseValidation

  Spec.describe "Includes correct files in tarballs" do
    removeIgnoredTarballFiles
    copySourceFiles

  Spec.describe "API pipelines run correctly" $ Spec.around withCleanEnv do
    Spec.it "Publish a package successfully" \{ workdir, failurePlan, index, metadata, storageDir, githubDir } -> do
      logs <- liftEffect (Ref.new [])

      let
        testEnv =
          { workdir
          , failurePlan
          , logs
          , index
          , metadata
          , pursuitExcludes: Set.empty
          , username: "jon"
          , storage: storageDir
          , github: githubDir
          }

      result <- Assert.Run.runTestEffects testEnv $ Except.runExcept do
        -- We'll publish effect@4.0.0 from the fixtures/github-packages
        -- directory, which has a bower.json manifest.
        let
          name = Utils.unsafePackageName "effect"
          version = Utils.unsafeVersion "4.0.0"
          ref = "v4.0.0"
          publishArgs =
            { compiler: Just $ Utils.unsafeVersion "0.15.10"
            , location: Just $ GitHub { owner: "purescript", repo: "purescript-effect", subdir: Nothing }
            , name
            , ref
            , version: version
            , resolutions: Nothing
            }

        -- First, we publish the package.
        void $ API.publish publishArgs

        -- Then, we can check that it did make it to "Pursuit" as expected
        Pursuit.getPublishedVersions name >>= case _ of
          Right versions | isJust (Map.lookup version versions) -> pure unit
          Right _ -> Except.throw $ "Expected " <> formatPackageVersion name version <> " to be published to Pursuit."
          Left err -> Except.throw $ "Failed to get published versions: " <> err

        -- As well as to the storage backend
        Storage.query name >>= \versions ->
          unless (Set.member version versions) do
            Except.throw $ "Expected " <> formatPackageVersion name version <> " to be published to registry storage."

        -- We should verify the resulting metadata file is correct
        Metadata effectMetadata <- Registry.readMetadata name >>= case _ of
          Nothing -> Except.throw $ "Expected " <> PackageName.print name <> " to be in metadata."
          Just m -> pure m

        case Map.lookup version effectMetadata.published of
          Nothing -> Except.throw $ "Expected " <> formatPackageVersion name version <> " to be in metadata."
          Just published -> do
            let many' = NonEmptyArray.toArray published.compilers
            -- Only 0.15.10 is expected because prelude only has 0.15.10 in metadata,
            -- so the solver cannot find a solution for 0.15.11
            let expected = map Utils.unsafeVersion [ "0.15.10" ]
            unless (many' == expected) do
              Except.throw $ "Expected " <> formatPackageVersion name version <> " to have a compiler matrix of " <> Utils.unsafeStringify (map Version.print expected) <> " but got " <> Utils.unsafeStringify (map Version.print many')

        -- Finally, we can verify that publishing the package again should fail
        -- since it already exists.
        Except.runExcept (API.publish publishArgs) >>= case _ of
          Left _ -> pure unit
          Right _ -> Except.throw $ "Expected publishing " <> formatPackageVersion name version <> " twice to fail."

      case result of
        Left exn -> do
          recorded <- liftEffect (Ref.read logs)
          Console.error $ String.joinWith "\n" (map (\(Tuple _ msg) -> msg) recorded)
          Assert.fail $ "Got an Aff exception! " <> Aff.message exn
        Right (Left err) -> do
          recorded <- liftEffect (Ref.read logs)
          Console.error $ String.joinWith "\n" (map (\(Tuple _ msg) -> msg) recorded)
          Assert.fail $ "Expected to publish effect@4.0.0 but got error: " <> err
        Right (Right _) -> pure unit

    Spec.describe "Publication retry reconciliation" do
      -- Storage, metadata, and the manifest index are the only durable writes in
      -- the publication pipeline. Failures before storage leave no state to
      -- reconcile; failures after the manifest happen after core publication is
      -- complete. These cases cover each partial durable state plus an upload
      -- whose write succeeds but whose response is ambiguous.
      Spec.it "Reconciles every partial durable publication state" \env -> do
        runPipelineAssertion env do
          Run.liftEffect $ Ref.write
            [ Assert.Run.FailStorageUploadAfterWrite
            , Assert.Run.FailMetadataWrite
            , Assert.Run.FailManifestWrite
            ]
            env.failurePlan

          -- The upload becomes durable before reporting failure. Publication
          -- verifies it and continues until the planned metadata failure.
          Except.runExcept (API.publish effectPublishArgs) >>= case _ of
            Left error | String.contains (Pattern "Injected metadata write failure") error -> pure unit
            Left error -> Except.throw $ "Expected an injected metadata failure but got: " <> error
            Right _ -> Except.throw "Expected metadata writing to fail."

          assertPublicationState effectName effectVersion { storage: true, metadata: false, manifest: false }

          -- The first retry reuses storage and repairs metadata, then encounters
          -- the planned manifest failure at the next durable boundary.
          Except.runExcept (API.publish effectPublishArgs) >>= case _ of
            Left error | String.contains (Pattern "Injected manifest write failure") error -> pure unit
            Left error -> Except.throw $ "Expected an injected manifest failure but got: " <> error
            Right _ -> Except.throw "Expected manifest writing to fail."

          assertPublicationState effectName effectVersion { storage: true, metadata: true, manifest: false }

          let storedTarballPath = Path.concat [ env.storageDir, "effect-4.0.0.tar.gz" ]
          storedTarball <- Run.liftAff $ FS.Aff.readFile storedTarballPath
          Run.liftAff $ FS.Aff.writeTextFile UTF8 storedTarballPath "corrupted tarball"
          Except.runExcept (API.publish effectPublishArgs) >>= case _ of
            Left error | String.contains (Pattern "Integrity check failed") error -> pure unit
            Left error -> Except.throw $ "Expected an existing tarball integrity failure but got: " <> error
            Right _ -> Except.throw "Expected retrying with a corrupted stored tarball to fail."
          assertPublicationState effectName effectVersion { storage: true, metadata: true, manifest: false }
          Run.liftAff $ FS.Aff.writeFile storedTarballPath storedTarball

          let sourceDir = Path.concat [ env.githubDir, "effect-4.0.0" ]
          let bowerPath = Path.concat [ sourceDir, "bower.json" ]
          let licensePath = Path.concat [ sourceDir, "LICENSE" ]
          originalBower <- Run.liftAff $ FS.Aff.readTextFile UTF8 bowerPath
          originalLicense <- Run.liftAff $ FS.Aff.readTextFile UTF8 licensePath
          mitLicense <- Run.liftAff $ FS.Aff.readTextFile UTF8 (Path.concat [ env.githubDir, "slug-3.0.0", "LICENSE" ])
          Run.liftAff $ FS.Aff.writeTextFile UTF8 bowerPath $ String.replace (Pattern "BSD-3-Clause") (Replacement "MIT") originalBower
          Run.liftAff $ FS.Aff.writeTextFile UTF8 licensePath mitLicense
          Except.runExcept (API.publish effectPublishArgs) >>= case _ of
            Left error | String.contains (Pattern "source manifest differs from the manifest in the existing tarball") error -> pure unit
            Left error -> Except.throw $ "Expected a changed source manifest failure but got: " <> error
            Right _ -> Except.throw "Expected retrying from a changed source manifest to fail."
          assertPublicationState effectName effectVersion { storage: true, metadata: true, manifest: false }

          Run.liftAff $ FS.Aff.writeTextFile UTF8 bowerPath originalBower
          Run.liftAff $ FS.Aff.writeTextFile UTF8 licensePath originalLicense
          API.publish effectPublishArgs >>= case _ of
            Just _ -> pure unit
            Nothing -> Except.throw "Expected retrying the incomplete publication to report a newly completed publication."
          assertPublicationState effectName effectVersion { storage: true, metadata: true, manifest: true }
          resetPublicationState env

          -- Pre-existing, unverifiable storage must never be overwritten.
          Run.liftAff $ FS.Aff.writeTextFile UTF8 storedTarballPath "conflicting tarball"
          Except.runExcept (API.publish effectPublishArgs) >>= case _ of
            Left error | String.contains (Pattern "existing tarball in storage could not be verified") error -> pure unit
            Left error -> Except.throw $ "Expected an existing tarball verification failure but got: " <> error
            Right _ -> Except.throw "Expected publishing with a conflicting tarball to fail."

          assertPublicationState effectName effectVersion { storage: true, metadata: false, manifest: false }
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
        copyFixture "github-packages"
        -- We remove effect fixtures since the unit test publishes effect from
        -- scratch and will fail if it's already registered. We have these in
        -- fixtures for the separate integration tests.
        FS.Extra.remove $ Path.concat [ testFixtures, "registry-storage", "effect-4.0.0.tar.gz" ]
        FS.Extra.remove $ Path.concat [ testFixtures, "registry", "metadata", "effect.json" ]
        FS.Extra.remove $ Path.concat [ testFixtures, "registry-index", "ef" ]

      let
        readFixtures = do
          initialMetadata <- Registry.readAllMetadataFromDisk $ Path.concat [ testFixtures, "registry", "metadata" ]
          metadata <- liftEffect $ Ref.new initialMetadata
          initialIndex <- Registry.readManifestIndexFromDisk $ Path.concat [ testFixtures, "registry-index" ]
          index <- liftEffect $ Ref.new initialIndex
          pure { initialMetadata, metadata, initialIndex, index }

      fixtures <- readFixtures
        # Log.interpret (\(Log.Log _ _ next) -> pure next)
        # Except.catch (\err -> Run.liftAff (Aff.throwError (Aff.error err)))
        # Run.runBaseAff'

      failurePlan <- liftEffect $ Ref.new []
      liftEffect $ Process.chdir workdir
      pure
        { workdir
        , failurePlan
        , metadata: fixtures.metadata
        , initialMetadata: fixtures.initialMetadata
        , index: fixtures.index
        , initialIndex: fixtures.initialIndex
        , storageDir: Path.concat [ testFixtures, "registry-storage" ]
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

parseSourceManifestSpec :: Spec.Spec Unit
parseSourceManifestSpec = do
  Spec.it "Rejects deprecated SPDX identifiers in purs.json" do
    resourceEnv <- liftEffect Env.lookupResourceEnv
    Aff.bracket Tmp.mkTmpDir FS.Extra.remove \packageDir -> do
      let
        manifestPath = Path.concat [ packageDir, "purs.json" ]
        args =
          { packageDir
          , name: Utils.unsafePackageName "registry-lib"
          , version: Utils.unsafeVersion "0.0.1"
          , ref: "v0.0.1"
          , location: GitHub { owner: "purescript", repo: "registry-dev", subdir: Nothing }
          }

      FS.Aff.writeTextFile UTF8 manifestPath
        """{"name":"registry-lib","version":"0.0.1","license":"AGPL-3.0","location":{"githubOwner":"purescript","githubRepo":"registry-dev"},"ref":"v0.0.1","dependencies":{"prelude":">=6.0.0 <7.0.0"}}"""

      result <-
        API.parseSourceManifest args
          # Env.runResourceEnv resourceEnv
          # Log.interpret (\(Log.Log _ _ next) -> pure next)
          # Except.runExcept
          # Run.runBaseAff'

      case result of
        Left err ->
          unless (String.contains (Pattern "license field is not canonical") err) do
            Assert.fail $ "Expected a canonical license error, but got: " <> err
        Right _ ->
          Assert.fail "Expected parseSourceManifest to reject deprecated SPDX identifiers"

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

licenseValidation :: Spec.Spec Unit
licenseValidation = do
  let
    fixtures = Path.concat [ "app", "fixtures", "licenses", "halogen-hooks" ]
    deprecatedFixture = Path.concat [ "app", "fixtures", "licenses", "deprecated-agpl" ]
    ambiguousFixture = Path.concat [ "app", "fixtures", "licenses", "ambiguous-gfdl" ]

  Spec.describe "validateLicense" do
    Spec.it "Passes when manifest license covers all detected licenses" do
      -- The halogen-hooks fixture has MIT in LICENSE and Apache-2.0 in package.json
      let manifestLicense = unsafeLicense "MIT AND Apache-2.0"
      result <- Assert.Run.runBaseEffects $ validateLicense fixtures manifestLicense
      Assert.shouldEqual Nothing result

    Spec.it "Fails when manifest license does not cover a detected license" do
      -- Manifest says MIT only, but Apache-2.0 is also in package.json
      let manifestLicense = unsafeLicense "MIT"
      result <- Assert.Run.runBaseEffects $ validateLicense fixtures manifestLicense
      case result of
        Just (LicenseMismatch { detected }) ->
          -- Should detect that Apache-2.0 is not covered
          Assert.shouldContain (map License.print detected) "Apache-2.0"
        _ ->
          Assert.fail "Expected LicenseMismatch error"

    Spec.it "Fails when manifest has completely different license" do
      -- Manifest says BSD-3-Clause, but fixture has MIT and Apache-2.0
      let manifestLicense = unsafeLicense "BSD-3-Clause"
      result <- Assert.Run.runBaseEffects $ validateLicense fixtures manifestLicense
      case result of
        Just (LicenseMismatch { manifest: ml, detected }) -> do
          Assert.shouldEqual "BSD-3-Clause" (License.print ml)
          -- Both MIT and Apache-2.0 should be in the detected licenses
          Assert.shouldContain (map License.print detected) "MIT"
          Assert.shouldContain (map License.print detected) "Apache-2.0"
        _ ->
          Assert.fail "Expected LicenseMismatch error"

    Spec.it "Passes when manifest uses OR conjunction" do
      -- OR conjunction is also valid - means either license applies
      let manifestLicense = unsafeLicense "MIT OR Apache-2.0"
      result <- Assert.Run.runBaseEffects $ validateLicense fixtures manifestLicense
      Assert.shouldEqual Nothing result

    Spec.it "Canonicalizes deterministic deprecated detected licenses during validation" do
      let manifestLicense = unsafeLicense "MIT"
      result <- Assert.Run.runBaseEffects $ validateLicense deprecatedFixture manifestLicense
      case result of
        Just (LicenseMismatch { detected }) ->
          Assert.shouldContain (map License.print detected) "AGPL-3.0-only"
        _ ->
          Assert.fail "Expected LicenseMismatch error"

    Spec.it "Preserves ambiguous deprecated detected licenses during validation" do
      let manifestLicense = unsafeLicense "MIT"
      result <- Assert.Run.runBaseEffects $ validateLicense ambiguousFixture manifestLicense
      case result of
        Just (LicenseMismatch { detected }) ->
          Assert.shouldContain (map License.print detected) "GFDL-1.3"
        _ ->
          Assert.fail "Expected LicenseMismatch error"

unsafeLicense :: String -> License
unsafeLicense str = unsafeFromRight $ License.parse str
