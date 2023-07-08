module Test.Registry.App.API (spec) where

import Registry.App.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.String.NonEmpty as NonEmptyString
import Effect.Aff as Aff
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.App.API as API
import Registry.App.Legacy.Types (RawPackageName(..))
import Registry.Constants as Constants
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Tmp as Tmp
import Registry.PackageName as PackageName
import Registry.Test.Assert as Assert
import Registry.Test.Assert.Run (TEST_EFFECTS)
import Registry.Test.Assert.Run as Assert.Run
import Registry.Test.Utils as Utils
import Registry.Version as Version
import Run (EFFECT, Run)
import Run as Run
import Test.Spec (ComputationType)
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Verifies build plans" do
    checkBuildPlanToResolutions

  Spec.describe "Includes correct files in tarballs" do
    removeIgnoredTarballFiles
    copySourceFiles

  Spec.describe "API pipelines run correctly" do
    Spec.it "Publish" do
      -- FIXME: The API pipeline will actually clone 'effect', but we probably
      -- want a minimal fixture instead (?)
      --
      -- FIXME: Gotta have a registry available in order to "publish" packages
      -- which have dependencies.
      { index, metadata } <- Assert.Run.readFixtures
      cwd <- liftEffect Process.cwd
      liftEffect $ Process.chdir ".."
      result <- Assert.Run.runTestEffects { index, metadata, username: "jon" } $ API.publish API.Current
        { compiler: Utils.unsafeVersion "0.15.9"
        , location: Just $ GitHub { owner: "purescript", repo: "purescript-effect", subdir: Nothing }
        , name: Utils.unsafePackageName "effect"
        , ref: "v4.0.0"
        , resolutions: Nothing
        }
      liftEffect $ Process.chdir cwd
      case result of
        Left err -> Aff.throwError (Aff.error err)
        Right _ -> pure unit

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
copySourceFiles = Spec.hoistSpec identity hoistFn $ Spec.before runBefore do
  let
    goodDirectories = [ "src" ]
    goodFiles = [ "purs.json", "README.md", "LICENSE", Path.concat [ "src", "Main.purs" ], Path.concat [ "src", "Main.js" ] ]

  Spec.it "Only copies always-included files by default" \{ source, destination, writeDirectories, writeFiles } -> do
    writeDirectories (goodDirectories <> Constants.ignoredDirectories <> [ "test" ])
    writeFiles (goodFiles <> Constants.ignoredFiles <> [ Path.concat [ "test", "Main.purs" ] ])

    API.copyPackageSourceFiles Nothing { source, destination }

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
      userFiles = NonEmptyArray.fromArray =<< sequence [ NonEmptyString.fromString "test/**/*.purs" ]
      testDir = [ "test" ]
      testFiles = [ Path.concat [ "test", "Main.purs" ], Path.concat [ "test", "Test.purs" ] ]

    writeDirectories (goodDirectories <> testDir)
    writeFiles (goodFiles <> testFiles)

    API.copyPackageSourceFiles userFiles { source, destination }

    paths <- FastGlob.match destination [ "**/*" ]

    let acceptedPaths = goodDirectories <> goodFiles <> testDir <> testFiles

    for_ acceptedPaths \path -> do
      paths.succeeded `Assert.Run.shouldContain` path
  where
  hoistFn :: forall a. ComputationType -> Run TEST_EFFECTS a -> Aff a
  hoistFn _ op = do
    { metadata, index } <- Assert.Run.readFixtures
    Assert.Run.runTestEffects { metadata, index, username: "jon" } op >>= case _ of
      Left err -> Aff.throwError $ Aff.error err
      Right a -> pure a

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
