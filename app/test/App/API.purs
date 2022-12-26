module Test.Registry.App.API (spec) where

import Registry.App.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.String.NonEmpty as NonEmptyString
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.API as API
import Registry.App.Legacy.Types (RawPackageName(..))
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Tmp as Tmp
import Registry.PackageName as PackageName
import Registry.Test.Assert.Run as Assert.Run
import Registry.Version as Version
import Run (EFFECT, Run)
import Run as Run
import Test.Assert as Assert
import Test.Spec as Spec
import Test.Utils as Utils

spec :: Spec.Spec Unit
spec = do
  Spec.describe "Verifies build plans" do
    checkBuildPlanToResolutions

  Spec.describe "Includes correct files in tarballs" do
    removeIgnoredTarballFiles
    copySourceFiles

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

    writeDirectories (goodDirectories <> API.ignoredDirectories)
    writeFiles (goodFiles <> API.ignoredFiles)

    API.removeIgnoredTarballFiles tmp

    paths <- FastGlob.match tmp [ "**/*" ]

    let
      ignoredPaths = API.ignoredDirectories <> API.ignoredFiles
      acceptedPaths = goodDirectories <> goodFiles

    for_ ignoredPaths \path ->
      paths.succeeded `Assert.shouldNotContain` path

    for_ acceptedPaths \path -> do
      paths.succeeded `Assert.shouldContain` path
  where
  runBefore = do
    tmp <- liftEffect Tmp.mkTmpDir

    let
      inTmp :: FilePath -> FilePath
      inTmp path = Path.concat [ tmp, path ]

      writeDirectories :: Array FilePath -> _
      writeDirectories = traverse_ (FS.Extra.ensureDirectory <<< inTmp)

      writeFiles :: Array FilePath -> _
      writeFiles = traverse_ (\path -> FS.Aff.writeTextFile UTF8 (inTmp path) "<test>")

    pure { tmp, writeDirectories, writeFiles }

copySourceFiles :: Spec.Spec Unit
copySourceFiles = Spec.hoistSpec identity (\_ -> Assert.Run.runTest) $ Spec.before runBefore do
  let
    goodDirectories = [ "src" ]
    goodFiles = [ "purs.json", "README.md", "LICENSE", Path.concat [ "src", "Main.purs" ], Path.concat [ "src", "Main.js" ] ]

  Spec.it "Only copies always-included files by default" \{ source, destination, writeDirectories, writeFiles } -> do
    writeDirectories (goodDirectories <> API.ignoredDirectories <> [ "test" ])
    writeFiles (goodFiles <> API.ignoredFiles <> [ Path.concat [ "test", "Main.purs" ] ])

    API.copyPackageSourceFiles Nothing { source, destination }

    paths <- Run.liftAff $ FastGlob.match destination [ "**/*" ]

    let
      acceptedPaths = goodDirectories <> goodFiles
      ignoredPaths = API.ignoredDirectories <> API.ignoredFiles

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

    paths <- liftAff $ FastGlob.match destination [ "**/*" ]

    let acceptedPaths = goodDirectories <> goodFiles <> testDir <> testFiles

    for_ acceptedPaths \path -> do
      paths.succeeded `Assert.Run.shouldContain` path

  where
  runBefore :: forall r. Run (EFFECT + r) _
  runBefore = do
    tmp <- Run.liftEffect Tmp.mkTmpDir
    destTmp <- Run.liftEffect Tmp.mkTmpDir

    let
      inTmp :: FilePath -> FilePath
      inTmp path = Path.concat [ tmp, path ]

      writeDirectories :: Array FilePath -> _
      writeDirectories = Run.liftAff <<< traverse_ (FS.Extra.ensureDirectory <<< inTmp)

      writeFiles :: Array FilePath -> _
      writeFiles = Run.liftAff <<< traverse_ (\path -> FS.Aff.writeTextFile UTF8 (inTmp path) "<test>")

    pure { source: tmp, destination: destTmp, writeDirectories, writeFiles }
