module Test.Main where

import Registry.Prelude

import Data.Argonaut.Core (stringify, stringifyWithIndent)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Codec.Argonaut (JsonDecodeError, decode, encode)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.String.NonEmpty as NES
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Exception
import Foreign.GitHub (IssueNumber(..))
import Foreign.Jsonic as Jsonic
import Foreign.Object as Object
import Foreign.SPDX as SPDX
import Foreign.SemVer as SemVer
import Node.FS.Aff as FS
import Registry.API as API
import Registry.Codec (parseJson)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..), Operation(..), Repo(..), manifestCodec)
import Registry.Scripts.LegacyImport.Bowerfile (Bowerfile(..), bowerfileCodec)
import Test.Foreign.Jsonic (jsonic)
import Test.Foreign.Licensee (licensee)
import Test.Registry.Index as Registry.Index
import Test.Registry.Scripts.LegacyImport.Stats (errorStats)
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
    pure $ map (\manifestFile -> packageDir <> "/" <> manifestFile) manifests

  runSpec' (defaultConfig { timeout = Just $ Milliseconds 10_000.0 }) [ consoleReporter ] do
    Spec.describe "API" do
      Spec.describe "Checks" do
        Spec.describe "Good package names" goodPackageName
        Spec.describe "Bad package names" badPackageName
        Spec.describe "Good SPDX licenses" goodSPDXLicense
        Spec.describe "Bad SPDX licenses" badSPDXLicense
        Spec.describe "Decode GitHub event to Operation" decodeEventsToOps
        Spec.describe "SemVer" semVer
    Spec.describe "Bowerfile" do
      Spec.describe "Parses" do
        Spec.describe "Good bower files" goodBowerfiles
      Spec.describe "Does not parse" do
        Spec.describe "Bad bower files" badBowerfiles
      Spec.describe "Encoding" bowerFileEncoding
    Spec.describe "Jsonic" jsonic
    Spec.describe "Licensee" licensee
    Spec.describe "Manifest" do
      Spec.describe "Encoding" manifestEncoding
      Spec.describe "Encoding examples" (manifestExamplesRoundtrip manifestExamplePaths)
    Spec.describe "Error Stats" errorStats
    Spec.describe "Registry Index" do
      Registry.Index.spec registryEnv

-- | Check all the example Manifests roundtrip (read+write) through PureScript
manifestExamplesRoundtrip :: Array FilePath -> Spec.Spec Unit
manifestExamplesRoundtrip paths = for_ paths \manifestPath -> Spec.it ("Roundrip check for " <> show manifestPath) do
  -- Now we read every manifest to our purescript type
  manifestStr <- FS.readTextFile UTF8 manifestPath
  case (jsonParser manifestStr >>= (lmap CA.printJsonDecodeError <<< CA.decode manifestCodec)) of
    Left err -> do
      error $ "Got error while parsing manifest"
      throwError $ Exception.error err
    Right (manifest :: Manifest) -> do
      -- And if that works, we then try to convert them back to JSON, and
      -- error out if any differ
      let newManifestStr = stringifyWithIndent 2 $ CA.encode manifestCodec manifest
      manifestStr `Assert.shouldEqual` newManifestStr

manifestEncoding :: Spec.Spec Unit
manifestEncoding = do
  let
    checkRoundtrip manifest str = case parseJson str >>= CA.decode manifestCodec of
      Left _ -> false
      Right manifest' -> manifest == manifest'

    roundTrip m@(Manifest manifest) =
      Spec.it (PackageName.print manifest.name <> " " <> SemVer.raw manifest.version) do
        stringify (encode manifestCodec m) `Assert.shouldSatisfy` checkRoundtrip m

  roundTrip Fixtures.ab.v1a
  roundTrip Fixtures.ab.v1b
  roundTrip Fixtures.ab.v2
  roundTrip Fixtures.abc.v1
  roundTrip Fixtures.abc.v2
  roundTrip Fixtures.abcd.v1
  roundTrip Fixtures.abcd.v2

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
        { packageName: fromRight' (\_ -> unsafeCrashWith "Expected Right") (PackageName.parse "something")
        , updateRef: "v1.2.3"
        , fromBower: false
        }

    res <- API.readOperation "test/fixtures/issue_comment.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber operation

  Spec.it "decodes an Addition operation" do
    let
      issueNumber = IssueNumber 149
      operation = Addition
        { packageName: fromRight' (\_ -> unsafeCrashWith "Expected Right") (PackageName.parse "prelude")
        , newRef: "v5.0.0"
        , fromBower: true
        , addToPackageSet: true
        , newPackageLocation: GitHub { subdir: Nothing, owner: "purescript", repo: "purescript-prelude" }
        }

    res <- API.readOperation "test/fixtures/issue_created.json"
    res `Assert.shouldEqual` API.DecodedOperation issueNumber operation

semVer :: Spec.Spec Unit
semVer = do
  let
    parseSemVer str = Spec.it ("Parse SemVer " <> str) do
      (SemVer.version <$> SemVer.parseSemVer str) `Assert.shouldSatisfy` isJust

  parseSemVer "v1.2.3"
  parseSemVer "1.2.3-rc2"
  parseSemVer "0.1.2+r2"

  let
    parseRange range expected = Spec.it ("Parse Range " <> show range <> " into " <> show expected) do
      (SemVer.printRange <$> SemVer.parseRange range) `Assert.shouldSatisfy` case _ of
        Just parsed -> parsed == expected
        Nothing -> false

  parseRange "^1.3.4" ">=1.3.4 <2.0.0-0"

goodBowerfiles :: Spec.Spec Unit
goodBowerfiles = do
  let
    parse :: String -> Either JsonDecodeError Bowerfile
    parse = Jsonic.parseJson >=> CA.decode bowerfileCodec

    parseBowerfile' str = Spec.it str do
      parse str `Assert.shouldSatisfy` isRight

    parseBowerfile = parseBowerfile' <<< stringify

    simpleFile =
      encode (CAR.object "simpleFile" { version: CA.string, license: CA.string })
        { version: "v1.0.0", license: "MIT" }
    goodBowerfile =
      encode (CAR.object "goodBowerFile" { version: CA.string, license: CA.string, dependencies: CAR.object "dependencies" {} })
        { version: "v1.0.0", license: "", dependencies: {} }
    extraPropsBowerfile =
      encode (CAR.object "extraPropsBowerFile" { extra: CA.string, license: CA.string, version: CA.string })
        { extra: "value"
        , license: "not a license"
        , version: "v1.1.1"
        }
    nonSemverBowerfile =
      encode
        (CAR.object "nonSemverBowerfile"
          { version: CA.string
          , license: CA.string
          , dependencies: CAR.object "also" { also: CA.string }
          , devDependencies: CAR.object "lastly" { lastly: CA.string }
          }
        )
        { version: "notsemver"
        , license: ""
        , dependencies: { also: "not semver" }
        , devDependencies: { lastly: "🍝" }
        }
    completeBowerfile =
      encode
        (CAR.object "completeBowerfile"
          { version: CA.string
          , license: CA.array CA.string
          , dependencies: CAR.object "dependencies" { "other-package": CA.string, "another-package": CA.string }
          , devDependencies: CAR.object "devDependencies" { "dev-dep": CA.string }
          }
        )
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
    parse :: String -> Either JsonDecodeError Bowerfile
    parse = Jsonic.parseJson >=> decode bowerfileCodec

    failParseBowerfile' str = Spec.it str do
      parse str `Assert.shouldNotSatisfy` isRight

    failParseBowerfile = failParseBowerfile' <<< stringify

    wrongLicenseFormat =
      encode (CAR.object "simpleFile" { version: CA.string, license: CA.boolean })
        { version: "", license: true }

    wrongDependenciesFormat =
      encode (CAR.object "simpleFile" { version: CA.string, license: CA.string, dependencies: CA.array CA.int })
        { version: "", license: "", dependencies: ([] :: Array Int) }

    wrongDevDependenciesFormat =
      encode (CAR.object "simpleFile" { version: CA.string, license: CA.string, devDependencies: CA.array CA.int })
        { version: "", license: "", devDependencies: ([] :: Array Int) }

  failParseBowerfile wrongLicenseFormat
  failParseBowerfile wrongDependenciesFormat
  failParseBowerfile wrongDevDependenciesFormat

bowerFileEncoding :: Spec.Spec Unit
bowerFileEncoding = do
  Spec.it "Can be decoded" do
    let
      dependencies =
        Object.fromFoldable
          [ Tuple "dependency-first" "v1.0.0"
          , Tuple "dependency-second" "v2.0.0"
          ]
      devDependencies =
        Object.fromFoldable
          [ Tuple "devdependency-first" "v0.0.1"
          , Tuple "devdependency-second" "v0.0.2"
          ]
      description = Nothing
      bowerFile =
        Bowerfile { license: NEA.fromArray $ Array.catMaybes [ NES.fromString "MIT" ], dependencies, devDependencies, description }
    (decode bowerfileCodec $ encode bowerfileCodec bowerFile) `Assert.shouldContain` bowerFile

