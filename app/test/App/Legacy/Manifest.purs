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
