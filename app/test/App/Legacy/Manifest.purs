module Test.Registry.App.Legacy.Manifest (spec) where

import Registry.App.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Codec.JSON as CJ
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.Manifest (Manifest(..))
import Registry.Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "Bowerfile" bowerfileSpec
  Spec.describe "bowerfileToPursJson" bowerfileToPursJsonSpec
  Spec.describe "spagoDhallToPursJson" spagoDhallToPursJsonSpec
  Spec.describe "Legacy manifest codec" legacyManifestCodecSpec

bowerfileSpec :: Spec Unit
bowerfileSpec = do
  Spec.describe "Parses acceptable Bowerfiles" do
    let
      shouldParse :: String -> String -> Spec Unit
      shouldParse label input = Spec.it label do
        case parseJson Legacy.Manifest.bowerfileCodec input of
          Left err -> Assert.fail $ Array.fold
            [ "Failed to parse:\n"
            , input
            , "due to an error:\n"
            , CJ.DecodeError.print err
            ]
          Right _ -> pure unit

    shouldParse "Version and license only"
      """
      {
        "version": "v1.0.0",
        "license": "MIT"
      }
      """

    shouldParse "Empty dependencies and license"
      """
      { "version": "v1.0.0"
      , "license": ""
      , "dependencies": {}
      }
      """

    shouldParse "Contains extra fields"
      """
      { "extra": "value"
      , "license": "not a license"
      , "version": "v1.1.1"
      }
      """

    shouldParse "Has non-SemVer dependenices"
      """
      { "version": "notsemver"
      , "license": ""
      , "dependencies": { "also": "not semver" }
      , "devDependencies": { "lastly": "🍝" }
      }
    """

    shouldParse "Complete bowerfile"
      """
      { "version": "v1.0.1"
      , "license": [ "license" ]
      , "dependencies":
          { "other-package": "v0.0.1"
          , "another-package": "v10.0.1-rc1"
          }
      , "devDependencies":
          { "dev-dep": "v2.0.0" }
      }
      """

  Spec.describe "Does not parse unacceptable Bowerfiles" do
    let
      shouldNotParse :: String -> String -> Spec Unit
      shouldNotParse label input = Spec.it label do
        case parseJson Legacy.Manifest.bowerfileCodec input of
          Left _ -> pure unit
          Right _ -> Assert.fail $ "Parsed input that should have failed:\n" <> input

    shouldNotParse "Wrong license format"
      """
      { "version": ""
      , "license": true
      }
      """

    shouldNotParse "Wrong dependency format"
      """
      { "version": ""
      , "license": ""
      , "dependencies": []
      }
      """

bowerfileToPursJsonSpec :: Spec Unit
bowerfileToPursJsonSpec = do
  Spec.describe "Converts valid Bowerfiles" do
    Spec.it "Converts a simple bowerfile with MIT license" do
      let
        input =
          """
          { "license": "MIT"
          , "dependencies": { "purescript-prelude": "^6.0.0" }
          }
          """
      case parseJson Legacy.Manifest.bowerfileCodec input of
        Left err -> Assert.fail $ "Failed to parse bowerfile:\n" <> CJ.DecodeError.print err
        Right bowerfile -> case Legacy.Manifest.bowerfileToPursJson bowerfile of
          Left err -> Assert.fail $ "Failed to convert bowerfile:\n" <> err
          Right result -> do
            result.description `Assert.shouldEqual` Nothing

    Spec.it "Strips purescript- prefix from dependency names" do
      let
        input =
          """
          { "license": "BSD-3-Clause"
          , "dependencies": { "purescript-effect": "^4.0.0" }
          }
          """
      case parseJson Legacy.Manifest.bowerfileCodec input of
        Left err -> Assert.fail $ "Failed to parse bowerfile:\n" <> CJ.DecodeError.print err
        Right bowerfile -> case Legacy.Manifest.bowerfileToPursJson bowerfile of
          Left err -> Assert.fail $ "Failed to convert bowerfile:\n" <> err
          Right _ -> pure unit

  Spec.describe "Rejects invalid Bowerfiles" do
    Spec.it "Fails on missing license" do
      let
        input =
          """
          { "dependencies": { "purescript-prelude": "^6.0.0" }
          }
          """
      case parseJson Legacy.Manifest.bowerfileCodec input of
        Left _ -> pure unit
        Right bowerfile -> case Legacy.Manifest.bowerfileToPursJson bowerfile of
          Left _ -> pure unit
          Right _ -> Assert.fail "Expected conversion to fail for missing license"

spagoDhallToPursJsonSpec :: Spec Unit
spagoDhallToPursJsonSpec = do
  Spec.describe "Converts valid SpagoDhallJson" do
    Spec.it "Converts spago.dhall with license and dependencies" do
      let
        input =
          """
          { "license": "MIT"
          , "dependencies": [ "prelude", "effect" ]
          , "packages": { "prelude": { "version": "v6.0.0" }, "effect": { "version": "v4.0.0" } }
          }
          """
      case parseJson Legacy.Manifest.spagoDhallJsonCodec input of
        Left err -> Assert.fail $ "Failed to parse spago.dhall JSON:\n" <> CJ.DecodeError.print err
        Right spagoDhall -> case Legacy.Manifest.spagoDhallToPursJson spagoDhall of
          Left err -> Assert.fail $ "Failed to convert spago.dhall:\n" <> err
          Right result -> do
            result.description `Assert.shouldEqual` Nothing

    Spec.it "Handles purescript- prefix in package names" do
      let
        input =
          """
          { "license": "BSD-3-Clause"
          , "dependencies": [ "purescript-prelude" ]
          , "packages": { "purescript-prelude": { "version": "v6.0.0" } }
          }
          """
      case parseJson Legacy.Manifest.spagoDhallJsonCodec input of
        Left err -> Assert.fail $ "Failed to parse:\n" <> CJ.DecodeError.print err
        Right spagoDhall -> case Legacy.Manifest.spagoDhallToPursJson spagoDhall of
          Left err -> Assert.fail $ "Failed to convert:\n" <> err
          Right _ -> pure unit

    Spec.it "Handles versions without v prefix" do
      let
        input =
          """
          { "license": "Apache-2.0"
          , "dependencies": [ "prelude" ]
          , "packages": { "prelude": { "version": "6.0.0" } }
          }
          """
      case parseJson Legacy.Manifest.spagoDhallJsonCodec input of
        Left err -> Assert.fail $ "Failed to parse:\n" <> CJ.DecodeError.print err
        Right spagoDhall -> case Legacy.Manifest.spagoDhallToPursJson spagoDhall of
          Left err -> Assert.fail $ "Failed to convert:\n" <> err
          Right _ -> pure unit

  Spec.describe "Rejects invalid SpagoDhallJson" do
    Spec.it "Fails on missing license" do
      let
        input =
          """
          { "dependencies": [ "prelude" ]
          , "packages": { "prelude": { "version": "v6.0.0" } }
          }
          """
      case parseJson Legacy.Manifest.spagoDhallJsonCodec input of
        Left _ -> pure unit
        Right spagoDhall -> case Legacy.Manifest.spagoDhallToPursJson spagoDhall of
          Left _ -> pure unit
          Right _ -> Assert.fail "Expected conversion to fail for missing license"

    Spec.it "Fails when dependency is not in packages" do
      let
        input =
          """
          { "license": "MIT"
          , "dependencies": [ "prelude", "missing" ]
          , "packages": { "prelude": { "version": "v6.0.0" } }
          }
          """
      case parseJson Legacy.Manifest.spagoDhallJsonCodec input of
        Left err -> Assert.fail $ "Failed to parse:\n" <> CJ.DecodeError.print err
        Right spagoDhall -> case Legacy.Manifest.spagoDhallToPursJson spagoDhall of
          Left _ -> pure unit
          Right _ -> Assert.fail "Expected conversion to fail for missing package"

legacyManifestCodecSpec :: Spec Unit
legacyManifestCodecSpec = do
  Spec.describe "Parses manifests with optional ref" do
    Spec.it "Parses manifest without ref, using fallback" do
      let
        input =
          """
          { "name": "prelude"
          , "version": "1.0.0"
          , "license": "MIT"
          , "location": { "githubOwner": "purescript", "githubRepo": "purescript-prelude" }
          , "dependencies": {}
          }
          """
        fallbackRef = "v1.0.0"
      case parseJson (Legacy.Manifest.legacyManifestCodec fallbackRef) input of
        Left err -> Assert.fail $ "Failed to parse manifest without ref:\n" <> CJ.DecodeError.print err
        Right (Manifest m) -> m.ref `Assert.shouldEqual` fallbackRef

    Spec.it "Parses manifest with ref, ignoring fallback" do
      let
        input =
          """
          { "name": "prelude"
          , "version": "1.0.0"
          , "license": "MIT"
          , "location": { "githubOwner": "purescript", "githubRepo": "purescript-prelude" }
          , "ref": "abc123"
          , "dependencies": {}
          }
          """
        fallbackRef = "v1.0.0"
      case parseJson (Legacy.Manifest.legacyManifestCodec fallbackRef) input of
        Left err -> Assert.fail $ "Failed to parse manifest with ref:\n" <> CJ.DecodeError.print err
        Right (Manifest m) -> m.ref `Assert.shouldEqual` "abc123"

    Spec.it "Roundtrips manifest with ref" do
      let
        input =
          """
          { "name": "prelude"
          , "version": "1.0.0"
          , "license": "MIT"
          , "location": { "githubOwner": "purescript", "githubRepo": "purescript-prelude" }
          , "ref": "v1.0.0"
          , "dependencies": {}
          }
          """
        fallbackRef = "unused"
      case parseJson (Legacy.Manifest.legacyManifestCodec fallbackRef) input of
        Left err -> Assert.fail $ "Failed to parse:\n" <> CJ.DecodeError.print err
        Right manifest -> do
          let encoded = CJ.encode (Legacy.Manifest.legacyManifestCodec fallbackRef) manifest
          case CJ.decode (Legacy.Manifest.legacyManifestCodec fallbackRef) encoded of
            Left err -> Assert.fail $ "Failed to decode roundtripped manifest:\n" <> CJ.DecodeError.print err
            Right (Manifest m) -> m.ref `Assert.shouldEqual` "v1.0.0"
