module Test.Registry.API where

import Registry.Prelude

import Data.Array as Array
import Data.Set as Set
import Data.String as String
import Effect.Exception as Exn
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Node.FS.Stats (Stats(..))
import Node.Glob.Basic (expandGlobs)
import Node.Path as Path
import Registry.API (pickFiles)
import Registry.Json as Json
import Test.Spec as Spec
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions as Assert

testSegments :: Array FilePath
testSegments = [ "test-fixtures", "aff-5.1.2" ]

testPickFiles :: Spec.Spec Unit
testPickFiles = Spec.before runBefore do
  Spec.it "Includes expected files" \{ pathsIn, pathsOut } -> do
    let includes = [ "purs.json", "package.json", "README.md", "LICENSE" ]
    for_ includes \path -> do
      pathsIn `Assert.shouldContain` path
      pathsOut `Assert.shouldContain` path

  Spec.it "Does not include unexpected files" \{ pathsIn, pathsOut } -> do
    let excludes = [ ".gitignore", "bower.json", ".travis.yml" ]
    for_ excludes \path -> do
      pathsIn `Assert.shouldContain` path
      pathsOut `Assert.shouldNotContain` path

  Spec.it "Copies all contents of expected directories" \{ tmp } -> do
    let sizeDir dir = FS.stat (Path.concat dir) >>= \(Stats { size }) -> pure size

    inputDirs <- do
      src <- sizeDir $ Array.snoc testSegments "src"
      test <- sizeDir $ Array.snoc testSegments "test"
      pure { src, test }

    outputDirs <- do
      src <- sizeDir [ tmp, "src" ]
      test <- sizeDir [ tmp, "test" ]
      pure { src, test }

    inputDirs.src `shouldEqual` outputDirs.src
    inputDirs.test `shouldEqual` outputDirs.test
  where
  runBefore = do
    tmp <- liftEffect Tmp.mkTmpDir
    eitherManifest <- Json.readJsonFile $ Path.concat [ "test-fixtures", "aff-5.1.2", "purs.json" ]
    case eitherManifest of
      Left err ->
        throwError (Exn.error err)
      Right manifest -> do
        let testDirectory = Path.concat testSegments
        pickFiles { inputDirectory: testDirectory, outputDirectory: tmp } manifest
        let stripPrefix dir path = fromMaybe path $ String.stripPrefix (String.Pattern (dir <> Path.sep)) path
        pathsIn <- expandGlobs testDirectory [ "*", "**/*" ]
        pathsOut <- expandGlobs tmp [ "*", "**/*" ]
        pure
          { tmp
          , pathsIn: Set.map (stripPrefix testDirectory) pathsIn
          , pathsOut: Set.map (stripPrefix tmp) pathsOut
          }
