module Test.Main where

import Registry.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.String.NonEmpty as NES
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Exception
import Foreign.FastGlob as FastGlob
import Foreign.GitHub (IssueNumber(..))
import Foreign.Node.FS as FS.Extra
import Foreign.SPDX as SPDX
import Foreign.Tmp as Tmp
import Node.FS.Aff as FS
import Node.Path as Path
import Node.Process as Process
import Registry.API as API
import Registry.Json as Json
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..), Operation(..), Repo(..))
import Registry.Scripts.LegacyImport.Bowerfile (Bowerfile(..))
import Registry.Version (rawVersion) as Version
import Safe.Coerce (coerce)
import Test.Foreign.JsonRepair as Foreign.JsonRepair
import Test.Foreign.Licensee (licensee)
import Test.Registry.Hash as Registry.Hash
import Test.Registry.Index as Registry.Index
import Test.Registry.Scripts.LegacyImport.Stats (errorStats)
import Test.Registry.Version (testRange, testVersion) as Version
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import Test.Support.Manifest as Fixtures

main :: Effect Unit
main = launchAff_ do
  -- Setup the Registry Index for tests
  registryEnv <- Registry.Index.mkTestIndexEnv

  -- get Manifest examples paths
  let examplesDir = "../examples/"
  packages <- FS.readdir examplesDir
  manifestExamplePaths <- join <$> for packages \package -> do
    let packageDir = examplesDir <> package
    manifests <- FS.readdir packageDir
    pure $ map (\manifestFile -> Path.concat [ packageDir, manifestFile ]) manifests

  runSpec' (defaultConfig { timeout = Just $ Milliseconds 10_000.0 }) [ consoleReporter ] do
    Spec.describe "API" do
      Spec.describe "Checks" do
        Spec.describe "Good package names" goodPackageName
        Spec.describe "Bad package names" badPackageName
        Spec.describe "Good SPDX licenses" goodSPDXLicense
        Spec.describe "Bad SPDX licenses" badSPDXLicense
        Spec.describe "Decode GitHub event to Operation" decodeEventsToOps
      Spec.describe "Tarball" do
        filterUnsafeGlobs
        pickTarballFiles
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
    Spec.describe "Error Stats" errorStats
    Spec.describe "Registry Index" do
      Registry.Index.spec registryEnv
    Spec.describe "Hash" do
      Registry.Hash.testHash
    Spec.describe "Json" do
      Foreign.JsonRepair.testJsonRepair
    Spec.describe "Version" do
      Version.testVersion
    Spec.describe "Range" do
      Version.testRange

-- | Check all the example Manifests roundtrip (read+write) through PureScript
manifestExamplesRoundtrip :: Array FilePath -> Spec.Spec Unit
manifestExamplesRoundtrip paths = for_ paths \manifestPath -> Spec.it ("Roundrip check for " <> show manifestPath) do
  -- Now we read every manifest to our purescript type
  manifestStr <- FS.readTextFile UTF8 manifestPath
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
      let fields = manifest { dependencies = mapKeys PackageName.print manifest.dependencies }
      Spec.it (PackageName.print manifest.name <> " " <> Version.rawVersion manifest.version) do
        Json.roundtrip fields `Assert.shouldContain` fields

  roundTrip Fixtures.ab.v1a
  roundTrip Fixtures.ab.v1b
  roundTrip Fixtures.ab.v2
  roundTrip Fixtures.abc.v1
  roundTrip Fixtures.abc.v2
  roundTrip Fixtures.abcd.v1
  roundTrip Fixtures.abcd.v2

pickTarballFiles :: Spec.Spec Unit
pickTarballFiles = Spec.before runBefore do
  Spec.it "Picks correct files when packaging a tarball" \{ tmpFrom, tmpTo, writeDirectories, writeFiles } -> do
    let
      goodDirectories = [ "src" ]
      badDirectories = API.ignoredDirectories <> [ Path.concat [ "test", "src" ] ]
      goodFiles = [ "purs.json", "README.md", "LICENSE", Path.concat [ "src", "Main.purs" ], Path.concat [ "src", "Main.js" ] ]
      badFiles = API.ignoredFiles <> [ Path.concat [ "test", "Main.purs" ] ]

    writeDirectories (goodDirectories <> badDirectories)
    writeFiles (goodFiles <> badFiles)

    API.pickTarballFiles { from: tmpFrom, to: tmpTo, files: [] }

    paths <- FastGlob.match' [ "**/*" ] { cwd: Just tmpTo }

    let
      ignoredPaths = badDirectories <> badFiles
      acceptedPaths = goodDirectories <> goodFiles

    for_ paths \path -> do
      ignoredPaths `Assert.shouldNotContain` path
      acceptedPaths `Assert.shouldContain` path

  Spec.it "Accepts extra files provided via the files key" \{ tmpFrom, tmpTo, writeDirectories, writeFiles } -> do
    let
      goodDirectories = [ "src" ]
      additionalDirectories = [ "test", Path.concat [ "examples", "another", "dir" ] ]
      badDirectories = [ ".git" ]
      goodFiles = [ "README.md" ]
      badFiles = [ "package-lock.json" ]
      additionalFiles =
        [ Path.concat [ "test", "Main.js" ]
        , Path.concat [ "examples", "another", "dir", "test.tsx" ]
        ]

    writeDirectories (goodDirectories <> badDirectories <> additionalDirectories)
    writeFiles (goodFiles <> badFiles <> additionalFiles)

    let files = [ "test/**/*.js", "examples/**/*" ] <> badDirectories <> badFiles
    API.pickTarballFiles { from: tmpFrom, to: tmpTo, files }

    paths <- FastGlob.match' [ "**/*" ] { cwd: Just tmpTo }

    let
      alwaysBad = badDirectories <> badFiles
      acceptedBad = [ Path.concat [ "test", "Main.js" ], Path.concat [ "examples", "another", "dir", "test.tsx" ] ]
      alwaysGood = goodDirectories <> goodFiles

    for_ alwaysBad \bad -> do
      -- Even if files are accepted via the files key, if they are banned files
      -- they should be missing
      paths `Assert.shouldNotContain` bad

    -- Even though by default these files would not be included, the presence
    -- of the files key should include them
    for_ acceptedBad \accepted ->
      paths `Assert.shouldContain` accepted

    for_ alwaysGood \good ->
      paths `Assert.shouldContain` good
  where
  runBefore = do
    tmpFrom <- liftEffect Tmp.mkTmpDir
    tmpTo <- liftEffect Tmp.mkTmpDir

    let
      inTmp :: FilePath -> FilePath
      inTmp path = Path.concat [ tmpFrom, path ]

      writeDirectories :: Array FilePath -> _
      writeDirectories = traverse_ (FS.Extra.ensureDirectory <<< inTmp)

      writeFiles :: Array FilePath -> _
      writeFiles = traverse_ (\path -> FS.writeTextFile UTF8 (inTmp path) "<test>")

    pure { tmpFrom, tmpTo, writeDirectories, writeFiles }

filterUnsafeGlobs :: Spec.Spec Unit
filterUnsafeGlobs =
  Spec.it "Filters out unsafe globs" do
    cwd <- liftEffect Process.cwd
    let globs = [ "./../../*.purs", "../abc/**/*", "/root/.ssh/**", "./*", "abc/*" ]
    filtered <- API.filterUnsafeGlobs cwd globs
    filtered `Assert.shouldEqual` [ "./*", "abc/*" ]

goodPackageName :: Spec.Spec Unit
goodPackageName = do
  let
    parseName str res = Spec.it str do
      (PackageName.print <$> PackageName.parse str) `Assert.shouldEqual` (Right res)

  parseName "a" "a"
  parseName "some-dash" "some-dash"

badPackageName :: Spec.Spec Unit
badPackageName = do
  let
    failParse str err = Spec.it str do
      (PackageName.print <$> PackageName.parse str) `Assert.shouldSatisfy` case _ of
        Right _ -> false
        Left { error } -> error == err
  let startErr = "Package name should start with a lower case char or a digit"
  let midErr = "Package name can contain lower case chars, digits and non-consecutive dashes"
  let endErr = "Package name should end with a lower case char or digit"
  let manyDashes = "Package names cannot contain consecutive dashes"

  failParse "-a" startErr
  failParse "double--dash" manyDashes
  failParse "BIGLETTERS" startErr
  failParse "some space" midErr
  failParse "a-" endErr
  failParse "" startErr
  failParse "🍝" startErr

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

decodeEventsToOps :: Spec.Spec Unit
decodeEventsToOps = do
  Spec.it "decodes an Update operation" do
    let
      issueNumber = IssueNumber 43
      operation = Update
        { packageName: unsafeFromRight $ PackageName.parse "something"
        , updateRef: "v1.2.3"
        }

    res <- API.readOperation "test/fixtures/issue_comment.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber operation

  Spec.it "decodes an Addition operation" do
    let
      issueNumber = IssueNumber 149
      operation = Addition
        { packageName: unsafeFromRight $ PackageName.parse "prelude"
        , newRef: "v5.0.0"
        , newPackageLocation: GitHub { subdir: Nothing, owner: "purescript", repo: "purescript-prelude" }
        }

    res <- API.readOperation "test/fixtures/issue_created.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber operation

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
        { license: NEA.fromArray $ Array.catMaybes [ NES.fromString "MIT" ]
        , dependencies
        , description
        }
    Json.roundtrip bowerFile `Assert.shouldContain` bowerFile

