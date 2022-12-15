module Test.Main (main) where

import Registry.App.Prelude

import Data.Argonaut.Encode as Argonaut.Codecs
import Data.Argonaut.Parser as Argonaut.Parser
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut as CA
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.String.NonEmpty as NonEmptyString
import Data.Time.Duration (Milliseconds(..))
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.API as API
import Registry.App.CLI.Purs (CompilerFailure(..))
import Registry.App.CLI.Purs as Purs
import Registry.App.Json as Json
import Registry.App.Legacy.Manifest (Bowerfile(..))
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.App.Legacy.Types (RawPackageName(..), RawVersionRange(..))
import Registry.Foreign.FSExtra as FS.Extra
import Registry.Foreign.FastGlob as FastGlob
import Registry.Foreign.Octokit (IssueNumber(..))
import Registry.Foreign.Tmp as Tmp
import Registry.Operation (PackageOperation(..), PackageSetOperation(..))
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.Version as Version
import Safe.Coerce (coerce)
import Test.Assert as Assert
import Test.Registry.App.Auth as Auth
import Test.Registry.App.CLI.Licensee as Test.Licensee
import Test.Registry.App.CLI.Tar as Test.Tar
import Test.Registry.App.Legacy.LenientRange as Test.LenientRange
import Test.Registry.App.Legacy.LenientVersion as Test.LenientVersion
import Test.Registry.App.PackageIndex as Test.PackageIndex
import Test.Registry.App.PackageSets as Test.PackageSets
import Test.RegistrySpec as RegistrySpec
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = launchAff_ do
  -- Setup the Registry Index for tests
  registryEnv <- Test.PackageIndex.mkTestIndexEnv

  runSpec' (defaultConfig { timeout = Just $ Milliseconds 10_000.0 }) [ consoleReporter ] do
    Spec.describe "API" do
      Spec.describe "Checks" do
        Spec.describe "Decode GitHub event to Operation" decodeEventsToOps
        Spec.describe "Authenticated operations" Auth.spec
      Spec.describe "Tarball" do
        copySourceFiles
        removeIgnoredTarballFiles
      Spec.describe "Compilers" do
        compilerVersions
      Spec.describe "Resolutions" do
        checkBuildPlanToResolutions
    Spec.describe "Bowerfile" do
      Spec.describe "Parses" do
        Spec.describe "Good bower files" goodBowerfiles
      Spec.describe "Does not parse" do
        Spec.describe "Bad bower files" badBowerfiles
      Spec.describe "Encoding" bowerFileEncoding
    Spec.describe "Package Index" do
      Test.PackageIndex.spec registryEnv
    Spec.describe "Package Set" do
      Test.PackageSets.spec

    Spec.describe "CLI" do
      Spec.describe "Licensee" Test.Licensee.spec
      Spec.describe "Tar" Test.Tar.spec

    Spec.describe "Legacy" do
      Spec.describe "Lenient Version" Test.LenientVersion.spec
      Spec.describe "Lenient Range" Test.LenientRange.spec

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
copySourceFiles = RegistrySpec.toSpec $ Spec.before runBefore do
  let
    goodDirectories = [ "src" ]
    goodFiles = [ "purs.json", "README.md", "LICENSE", Path.concat [ "src", "Main.purs" ], Path.concat [ "src", "Main.js" ] ]

  Spec.it "Only copies always-included files by default" \{ source, destination, writeDirectories, writeFiles } -> do
    writeDirectories (goodDirectories <> API.ignoredDirectories <> [ "test" ])
    writeFiles (goodFiles <> API.ignoredFiles <> [ Path.concat [ "test", "Main.purs" ] ])

    API.copyPackageSourceFiles Nothing { source, destination }

    paths <- liftAff $ FastGlob.match destination [ "**/*" ]

    let
      acceptedPaths = goodDirectories <> goodFiles
      ignoredPaths = API.ignoredDirectories <> API.ignoredFiles

    for_ acceptedPaths \path -> do
      paths.succeeded `Assert.shouldContain` path

    for_ ignoredPaths \path -> do
      paths.succeeded `Assert.shouldNotContain` path

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
      paths.succeeded `Assert.shouldContain` path

  where
  runBefore = do
    tmp <- liftEffect Tmp.mkTmpDir
    destTmp <- liftEffect Tmp.mkTmpDir

    let
      inTmp :: FilePath -> FilePath
      inTmp path = Path.concat [ tmp, path ]

      writeDirectories :: Array FilePath -> _
      writeDirectories = liftAff <<< traverse_ (FS.Extra.ensureDirectory <<< inTmp)

      writeFiles :: Array FilePath -> _
      writeFiles = liftAff <<< traverse_ (\path -> FS.Aff.writeTextFile UTF8 (inTmp path) "<test>")

    pure { source: tmp, destination: destTmp, writeDirectories, writeFiles }

mkUnsafePackage :: String -> PackageName
mkUnsafePackage = unsafeFromRight <<< PackageName.parse

mkUnsafeVersion :: String -> Version
mkUnsafeVersion = unsafeFromRight <<< Version.parse

decodeEventsToOps :: Spec.Spec Unit
decodeEventsToOps = do
  Spec.it "decodes an Update operation" do
    let
      issueNumber = IssueNumber 43
      username = "Codertocat"
      operation = Publish
        { name: mkUnsafePackage "something"
        , ref: "v1.2.3"
        , compiler: mkUnsafeVersion "0.15.0"
        , resolutions: Just $ Map.fromFoldable [ mkUnsafePackage "prelude" /\ mkUnsafeVersion "1.0.0" ]
        , location: Nothing
        }

    res <- API.readOperation "test/_fixtures/update_issue_comment.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber username (Right operation)

  Spec.it "decodes an Addition operation" do
    let
      issueNumber = IssueNumber 149
      username = "Codertocat"
      operation = Publish
        { name: mkUnsafePackage "prelude"
        , ref: "v5.0.0"
        , location: Just $ GitHub { subdir: Nothing, owner: "purescript", repo: "purescript-prelude" }
        , compiler: mkUnsafeVersion "0.15.0"
        , resolutions: Just $ Map.fromFoldable [ mkUnsafePackage "prelude" /\ mkUnsafeVersion "1.0.0" ]
        }

    res <- API.readOperation "test/_fixtures/addition_issue_created.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber username (Right operation)

  Spec.it "decodes a Package Set Update operation" do
    let
      issueNumber = IssueNumber 149
      username = "Codertocat"
      operation = PackageSetUpdate
        { compiler: Nothing
        , packages: Map.fromFoldable
            [ mkUnsafePackage "aff" /\ Just (mkUnsafeVersion "7.0.0")
            , mkUnsafePackage "argonaut" /\ Nothing
            ]
        }

    res <- API.readOperation "test/_fixtures/package-set-update_issue_created.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber username (Left operation)

  Spec.it "decodes lenient JSON" do
    let
      operation = Publish
        { name: mkUnsafePackage "prelude"
        , ref: "v5.0.0"
        , location: Just $ GitHub { subdir: Nothing, owner: "purescript", repo: "purescript-prelude" }
        , compiler: mkUnsafeVersion "0.15.0"
        , resolutions: Nothing
        }

      rawOperation = preludeAdditionString

      parseJson = bimap CA.printJsonDecodeError Publish <<< CA.decode Operation.publishCodec <=< Argonaut.Parser.jsonParser

    parseJson (API.firstObject rawOperation) `Assert.shouldEqual` (Right operation)

goodBowerfiles :: Spec.Spec Unit
goodBowerfiles = do
  let
    parse :: String -> Either String Bowerfile
    parse = Json.parseJson Legacy.Manifest.bowerfileCodec

    parseBowerfile' str = Spec.it str do
      parse str `Assert.shouldSatisfy` isRight

    parseBowerfile = parseBowerfile' <<< Json.stringify

    simpleFile =
      Argonaut.Codecs.encodeJson
        { version: "v1.0.0"
        , license: "MIT"
        }

    goodBowerfile =
      Argonaut.Codecs.encodeJson
        { version: "v1.0.0"
        , license: ""
        , dependencies: {}
        }

    extraPropsBowerfile =
      Argonaut.Codecs.encodeJson
        { extra: "value"
        , license: "not a license"
        , version: "v1.1.1"
        }

    nonSemverBowerfile =
      Argonaut.Codecs.encodeJson
        { version: "notsemver"
        , license: ""
        , dependencies: { also: "not semver" }
        , devDependencies: { lastly: "🍝" }
        }

    completeBowerfile =
      Argonaut.Codecs.encodeJson
        { version: "v1.0.1"
        , license: [ "license" ]
        , dependencies:
            { "other-package": "v0.0.1"
            , "another-package": "v10.0.1-rc1"
            }
        , devDependencies:
            { "dev-dep": "v2.0.0" }
        }

  parseBowerfile goodBowerfile
  parseBowerfile simpleFile
  parseBowerfile extraPropsBowerfile
  parseBowerfile nonSemverBowerfile
  parseBowerfile completeBowerfile

badBowerfiles :: Spec.Spec Unit
badBowerfiles = do
  let
    parse :: String -> Either String Bowerfile
    parse = Json.parseJson Legacy.Manifest.bowerfileCodec

    failParseBowerfile' str = Spec.it str do
      parse str `Assert.shouldNotSatisfy` isRight

    failParseBowerfile = failParseBowerfile' <<< Json.stringify

    wrongLicenseFormat =
      Argonaut.Codecs.encodeJson { version: "", license: true }

    wrongDependenciesFormat =
      Argonaut.Codecs.encodeJson { version: "", license: "", dependencies: ([] :: Array Int) }

  failParseBowerfile wrongLicenseFormat
  failParseBowerfile wrongDependenciesFormat

bowerFileEncoding :: Spec.Spec Unit
bowerFileEncoding = do
  Spec.it "Can be decoded" do
    let
      dependencies =
        Map.fromFoldable $ map coerce
          [ Tuple "dependency-first" "v1.0.0"
          , Tuple "dependency-second" "v2.0.0"
          ]
      description = Nothing
      bowerFile = Bowerfile
        { license: [ "MIT" ]
        , dependencies
        , description
        }

    Json.decode Legacy.Manifest.bowerfileCodec (Json.encode Legacy.Manifest.bowerfileCodec bowerFile)
      `Assert.shouldContain` bowerFile

checkBuildPlanToResolutions :: Spec.Spec Unit
checkBuildPlanToResolutions = do
  Spec.it "buildPlanToResolutions produces expected resolutions file format" do
    Assert.shouldEqual generatedResolutions expectedResolutions
  where
  dependenciesDir = "testDir"

  resolutions = Map.fromFoldable
    [ Tuple (mkUnsafePackage "prelude") (mkUnsafeVersion "1.0.0")
    , Tuple (mkUnsafePackage "bifunctors") (mkUnsafeVersion "2.0.0")
    , Tuple (mkUnsafePackage "ordered-collections") (mkUnsafeVersion "3.0.0")
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

compilerVersions :: Spec.Spec Unit
compilerVersions = do
  traverse_ testVersion [ "0.13.0", "0.14.0", "0.14.7", "0.15.4" ]
  traverse_ testMissingVersion [ "0.13.1", "0.14.8" ]
  where
  testVersion version =
    Spec.it ("Calls compiler version " <> version) do
      Purs.callCompiler { command: Purs.Version, cwd: Nothing, version } >>= case _ of
        Left err -> case err of
          MissingCompiler ->
            Assert.fail "MissingCompiler"
          CompilationError errs ->
            Assert.fail ("CompilationError:\n" <> Purs.printCompilerErrors errs)
          UnknownError err' ->
            Assert.fail ("UnknownError: " <> err')
        Right stdout ->
          version `Assert.shouldEqual` stdout

  testMissingVersion version =
    Spec.it ("Handles failure when compiler is missing " <> version) do
      result <- Purs.callCompiler { command: Purs.Version, cwd: Nothing, version }
      case result of
        Left MissingCompiler -> pure unit
        _ -> Assert.fail "Should have failed with MissingCompiler"

preludeAdditionString :: String
preludeAdditionString =
  """
  Here's my new package!

  ```json
  {
    "name": "prelude",
    "ref": "v5.0.0",
    "location": {
      "githubOwner": "purescript",
      "githubRepo": "purescript-prelude"
    },
    "compiler": "0.15.0"
  }
  ```

  Thanks!
  """
