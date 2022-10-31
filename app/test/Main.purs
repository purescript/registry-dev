module Test.Main
  ( main
  ) where

import Registry.Prelude

import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Exception
import Foreign.FastGlob as FastGlob
import Foreign.GitHub (IssueNumber(..))
import Foreign.Node.FS as FS.Extra
import Foreign.Purs (CompilerFailure(..))
import Foreign.Purs as Purs
import Foreign.SPDX as SPDX
import Foreign.Tmp as Tmp
import Node.FS as FS
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Node.Process as Process
import Registry.API (copyPackageSourceFiles)
import Registry.API as API
import Registry.Json as Json
import Registry.Legacy.Manifest (Bowerfile(..))
import Registry.Operation (Operation(..))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (BuildPlan(..), Location(..), Manifest(..))
import Registry.Version (Version)
import Registry.Version as Version
import Safe.Coerce (coerce)
import Test.Assert as Assert
import Test.Fixture.Manifest as Fixture
import Test.Foreign.JsonRepair as Foreign.JsonRepair
import Test.Foreign.Licensee (licensee)
import Test.Foreign.Tar as Foreign.Tar
import Test.Registry.Index as Registry.Index
import Test.Registry.PackageSet as PackageSet
import Test.Registry.SSH as SSH
import Test.Registry.Solver as TestSolver
import Test.Registry.Version as TestVersion
import Test.RegistrySpec as RegistrySpec
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = launchAff_ do
  -- Setup the Registry Index for tests
  registryEnv <- Registry.Index.mkTestIndexEnv

  -- get Manifest examples paths
  let examplesDir = "examples"
  packages <- FS.Aff.readdir examplesDir
  manifestExamplePaths <- join <$> for packages \package -> do
    let packageDir = Path.concat [ examplesDir, package ]
    manifests <- FS.Aff.readdir packageDir
    pure $ map (\manifestFile -> Path.concat [ packageDir, manifestFile ]) manifests

  runSpec' (defaultConfig { timeout = Just $ Milliseconds 10_000.0 }) [ consoleReporter ] do
    Spec.describe "API" do
      Spec.describe "Checks" do
        Spec.describe "Good SPDX licenses" goodSPDXLicense
        Spec.describe "Bad SPDX licenses" badSPDXLicense
        Spec.describe "Decode GitHub event to Operation" decodeEventsToOps
        Spec.describe "Authenticated operations" SSH.spec
      Spec.describe "Tarball" do
        copySourceFiles
        removeIgnoredTarballFiles
      Spec.describe "Compilers" do
        compilerVersions
      Spec.describe "Resolutions" do
        checkBuildPlanToResolutions
        checkDependencyResolution
    Spec.describe "Bowerfile" do
      Spec.describe "Parses" do
        Spec.describe "Good bower files" goodBowerfiles
      Spec.describe "Does not parse" do
        Spec.describe "Bad bower files" badBowerfiles
      Spec.describe "Encoding" bowerFileEncoding
    Spec.describe "Licensee" licensee
    Spec.describe "Manifest" do
      Spec.describe "Encoding" manifestEncoding
      Spec.describe "Encoding examples" (manifestExamplesRoundtrip manifestExamplePaths)
    Spec.describe "Registry Index" do
      Registry.Index.spec registryEnv
    Spec.describe "Tar" do
      Foreign.Tar.tar
    Spec.describe "Json Repair" do
      Foreign.JsonRepair.testJsonRepair
    Spec.describe "Version" do
      TestVersion.testVersion
    Spec.describe "Range" do
      TestVersion.testRange
    Spec.describe "Solver" do
      TestSolver.spec
    Spec.describe "Glob" do
      safeGlob
    Spec.describe "Package Set" do
      PackageSet.spec

-- | Check all the example Manifests roundtrip (read+write) through PureScript
manifestExamplesRoundtrip :: Array FilePath -> Spec.Spec Unit
manifestExamplesRoundtrip paths = for_ paths \manifestPath -> Spec.it ("Roundrip check for " <> show manifestPath) do
  -- Now we read every manifest to our purescript type
  manifestStr <- FS.Aff.readTextFile UTF8 manifestPath
  case Json.parseJson manifestStr of
    Left err -> do
      error $ "Got error while parsing manifest"
      throwError $ Exception.error err
    Right (manifest :: Manifest) -> do
      -- And if that works, we then try to convert them back to JSON, and
      -- error out if any differ
      let newManifestStr = Json.printJson manifest
      manifestStr `Assert.shouldEqual` newManifestStr

manifestEncoding :: Spec.Spec Unit
manifestEncoding = do
  let
    roundTrip (Manifest manifest) = do
      Spec.it (PackageName.print manifest.name <> " " <> Version.rawVersion manifest.version) do
        Json.roundtrip manifest `Assert.shouldContain` manifest

  roundTrip Fixture.fixture
  roundTrip (Fixture.setDependencies [ Tuple "package-a" "1.0.0" ] Fixture.fixture)

safeGlob :: Spec.Spec Unit
safeGlob = do
  Spec.describe "Prevents directory traversals" do
    Spec.it "Directory traversal" do
      tmp <- liftEffect Tmp.mkTmpDir
      let outerFile = Path.concat [ tmp, "flake.nix" ]
      FS.Aff.writeTextFile UTF8 outerFile "<contents>"
      let innerDirectory = Path.concat [ tmp, "inner" ]
      FS.Extra.ensureDirectory innerDirectory
      { succeeded, failed } <- FastGlob.match innerDirectory [ "../flake.nix" ]
      succeeded `Assert.shouldEqual` []
      failed `Assert.shouldEqual` [ "../flake.nix" ]

    Spec.it "Symlink traversal" do
      tmp <- liftEffect Tmp.mkTmpDir
      let outerFile = Path.concat [ tmp, "shell.nix" ]
      FS.Aff.writeTextFile UTF8 outerFile "<contents>"
      let innerDirectory = Path.concat [ tmp, "inner" ]
      FS.Extra.ensureDirectory innerDirectory
      FS.Aff.symlink outerFile (Path.concat [ innerDirectory, "shell.nix" ]) FS.FileLink
      { succeeded, failed } <- FastGlob.match innerDirectory [ "./shell.nix" ]
      succeeded `Assert.shouldEqual` []
      failed `Assert.shouldEqual` [ "./shell.nix" ]

    -- A glob that is technically a directory traversal but which doesn't
    -- actually match any files won't throw an error since there are no results.
    Spec.it "Traversal to a non-existing file" do
      cwd <- liftEffect Process.cwd
      result <- FastGlob.match cwd [ "/var/www/root/shell.nix" ]
      unless (result == mempty) do
        Assert.fail $ "Expected no results, but received " <> show result

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

    copyPackageSourceFiles Nothing { source, destination }

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
      userFiles = Just [ "test/**/*.purs" ]
      testDir = [ "test" ]
      testFiles = [ Path.concat [ "test", "Main.purs" ], Path.concat [ "test", "Test.purs" ] ]

    writeDirectories (goodDirectories <> testDir)
    writeFiles (goodFiles <> testFiles)

    copyPackageSourceFiles userFiles { source, destination }

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

goodSPDXLicense :: Spec.Spec Unit
goodSPDXLicense = do
  let
    parseLicense str = Spec.it str do
      (SPDX.print <$> SPDX.parse str) `Assert.shouldSatisfy` isRight

  -- current licenses
  parseLicense "MIT"
  parseLicense "BSD-3-Clause"
  parseLicense "CC-BY-1.0"
  parseLicense "Apache-2.0"

  -- deprecated licenses
  parseLicense "GPL-3.0"
  parseLicense "AGPL-1.0"

  -- combinations
  parseLicense "LGPL-2.1 OR BSD-3-Clause AND MIT"
  parseLicense "MIT AND (LGPL-2.1+ AND BSD-3-Clause)"

  -- exceptions
  parseLicense "GPL-3.0 WITH GPL-3.0-linking-exception"

badSPDXLicense :: Spec.Spec Unit
badSPDXLicense = do
  let
    invalid str suggestion = "Invalid SPDX identifier: " <> str <> case suggestion of
      Nothing -> ""
      Just s -> "\nDid you mean " <> s <> "?"
    parseLicense str suggestion = Spec.it str do
      (SPDX.print <$> SPDX.parse str) `Assert.shouldSatisfy` case _ of
        Right _ -> false
        Left err -> err == invalid str suggestion

  -- common mistakes
  parseLicense "Apache" (Just "Apache-1.0")
  parseLicense "Apache-2" (Just "Apache-2.0")
  parseLicense "Apache 2" (Just "Apache-2.0")
  parseLicense "BSD-3" (Just "BSD-3-Clause")
  parseLicense "MIT AND BSD-3" Nothing

mkUnsafePackage :: String -> PackageName
mkUnsafePackage = unsafeFromRight <<< PackageName.parse

mkUnsafeVersion :: String -> Version
mkUnsafeVersion = unsafeFromRight <<< Version.parseVersion Version.Strict

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

    res <- API.readOperation "test/fixtures/update_issue_comment.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber username operation

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

    res <- API.readOperation "test/fixtures/addition_issue_created.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber username operation

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

    res <- API.readOperation "test/fixtures/package-set-update_issue_created.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber username operation

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

    Json.parseJson (API.firstObject rawOperation) `Assert.shouldEqual` (Right operation)

goodBowerfiles :: Spec.Spec Unit
goodBowerfiles = do
  let
    parse :: String -> Either String Bowerfile
    parse = Json.parseJson

    parseBowerfile' str = Spec.it str do
      parse str `Assert.shouldSatisfy` isRight

    parseBowerfile = parseBowerfile' <<< Json.stringify

    simpleFile = Json.encode { version: "v1.0.0", license: "MIT" }
    goodBowerfile = Json.encode { version: "v1.0.0", license: "", dependencies: {} }
    extraPropsBowerfile =
      Json.encode
        { extra: "value"
        , license: "not a license"
        , version: "v1.1.1"
        }
    nonSemverBowerfile =
      Json.encode
        { version: "notsemver"
        , license: ""
        , dependencies: { also: "not semver" }
        , devDependencies: { lastly: "🍝" }
        }
    completeBowerfile =
      Json.encode
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
    parse = Json.parseJson

    failParseBowerfile' str = Spec.it str do
      parse str `Assert.shouldNotSatisfy` isRight

    failParseBowerfile = failParseBowerfile' <<< Json.stringify

    wrongLicenseFormat =
      Json.encode { version: "", license: true }

    wrongDependenciesFormat =
      Json.encode
        { version: "", license: "", dependencies: ([] :: Array Int) }

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
    Json.roundtrip bowerFile `Assert.shouldContain` bowerFile

checkDependencyResolution :: Spec.Spec Unit
checkDependencyResolution = do
  Spec.it "Handles build plan with all dependencies resolved" do
    Assert.shouldEqual (API.getUnresolvedDependencies manifest exactBuildPlan) []
  Spec.it "Handles build plan with all dependencies resolved + extra" do
    Assert.shouldEqual (API.getUnresolvedDependencies manifest extraBuildPlan) []
  Spec.it "Handles build plan with resolution missing package" do
    Assert.shouldEqual (API.getUnresolvedDependencies manifest buildPlanMissingPackage) [ Left (packageTwoName /\ packageTwoRange) ]
  Spec.it "Handles build plan with resolution having package at wrong version" do
    Assert.shouldEqual (API.getUnresolvedDependencies manifest buildPlanWrongVersion) [ Right (packageTwoName /\ packageTwoRange /\ mkUnsafeVersion "7.0.0") ]
  where
  manifest@(Manifest { dependencies }) =
    Fixture.setDependencies [ Tuple "package-one" "2.0.0", Tuple "package-two" "3.0.0" ] Fixture.fixture

  packageTwoName = mkUnsafePackage "package-two"
  packageTwoRange = unsafeFromJust $ Map.lookup packageTwoName dependencies

  exactBuildPlan = Map.fromFoldable
    [ Tuple (mkUnsafePackage "package-one") (mkUnsafeVersion "2.0.0")
    , Tuple (mkUnsafePackage "package-two") (mkUnsafeVersion "3.0.0")
    ]

  extraBuildPlan = Map.fromFoldable
    [ Tuple (mkUnsafePackage "package-one") (mkUnsafeVersion "2.0.0")
    , Tuple (mkUnsafePackage "package-two") (mkUnsafeVersion "3.0.0")
    , Tuple (mkUnsafePackage "package-three") (mkUnsafeVersion "7.0.0")
    ]

  buildPlanMissingPackage = Map.fromFoldable
    [ Tuple (mkUnsafePackage "package-one") (mkUnsafeVersion "2.0.0")
    , Tuple (mkUnsafePackage "package-three") (mkUnsafeVersion "7.0.0")
    ]

  buildPlanWrongVersion = Map.fromFoldable
    [ Tuple (mkUnsafePackage "package-one") (mkUnsafeVersion "2.0.0")
    , Tuple (mkUnsafePackage "package-two") (mkUnsafeVersion "7.0.0")
    ]

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
    API.buildPlanToResolutions
      { buildPlan: BuildPlan { compiler: mkUnsafeVersion "0.14.2", resolutions: Just resolutions }
      , dependenciesDir
      }

  expectedResolutions = Map.fromFoldable do
    packageName /\ version <- (Map.toUnfoldable resolutions :: Array _)
    let
      bowerName = RawPackageName ("purescript-" <> PackageName.print packageName)
      path = Path.concat [ dependenciesDir, PackageName.print packageName <> "-" <> Version.printVersion version ]
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
