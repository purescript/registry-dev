module Test.Registry.App.Legacy.Manifest (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Registry.App.Legacy.Manifest as Legacy.Manifest
import Registry.Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "Bowerfile" bowerfileSpec

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
            , CA.printJsonDecodeError err
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
