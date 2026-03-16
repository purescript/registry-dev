module Test.Registry.Manifest (spec) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Codec.JSON as CJ
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.String as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Traversable (for)
import JSON as JSON
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.Manifest as Manifest
import Registry.Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Round-trips manifest fixtures" do
    let manifestFixturesPath = Path.concat [ "lib", "fixtures", "manifests" ]
    fixturePaths <- FS.Aff.readdir manifestFixturesPath
    fixtures <- for fixturePaths \path -> do
      rawManifest <- FS.Aff.readTextFile UTF8 $ Path.concat [ manifestFixturesPath, path ]
      pure { label: path, value: String.trim rawManifest }
    Assert.shouldRoundTrip "Manifest" Manifest.codec fixtures

  Spec.it "Decodes and canonicalizes historical manifests with deprecated SPDX identifiers" do
    for_ historicalManifests \{ label, input, expected } ->
      case JSON.parse input of
        Left err ->
          Assert.fail $ "Failed to parse test JSON for " <> label <> ": " <> err
        Right json ->
          case CJ.decode Manifest.codec json of
            Left err ->
              Assert.fail $ "Failed to decode historical manifest for " <> label <> ": " <> CJ.DecodeError.print err
            Right manifest ->
              Assert.shouldEqual expected (JSON.print $ CJ.encode Manifest.codec manifest)

historicalManifests :: Array { label :: String, input :: String, expected :: String }
historicalManifests =
  [ historicalManifest "AGPL-3.0" "jarilo" "1.0.1" "AGPL-3.0" "AGPL-3.0-only"
  , historicalManifest "eCos-2.0" "ecos" "1.2.3" "eCos-2.0" "GPL-2.0-or-later WITH eCos-exception-2.0"
  , historicalManifest "LGPL-3.0" "matrices" "5.0.0" "LGPL-3.0" "LGPL-3.0-only"
  , historicalManifest "LGPL-3.0+" "test-unit" "17.0.0" "LGPL-3.0+" "LGPL-3.0-or-later"
  , historicalManifest "GPL-3.0 AND MIT" "nano-id" "1.1.0" "GPL-3.0 AND MIT" "GPL-3.0-only AND MIT"
  , historicalManifest "LGPL-2.1 AND LGPL-2.1-only" "bookhound" "0.1.1" "LGPL-2.1 AND LGPL-2.1-only" "LGPL-2.1-only AND LGPL-2.1-only"
  , historicalManifest "wxWindows" "wx" "0.9.0" "wxWindows" "LGPL-2.0-or-later WITH WxWindows-exception-3.1"
  ]

historicalManifest :: String -> String -> String -> String -> String -> { label :: String, input :: String, expected :: String }
historicalManifest label name version historical canonical =
  { label
  , input: manifestJson historical name version
  , expected: manifestJson canonical name version
  }

manifestJson :: String -> String -> String -> String
manifestJson license name version =
  manifestTemplate
    # replace "__LICENSE__" license
    # replace "__VERSION__" version
    # replace "__NAME__" name

manifestTemplate :: String
manifestTemplate =
  """{"name":"__NAME__","version":"__VERSION__","license":"__LICENSE__","location":{"githubOwner":"purescript","githubRepo":"purescript-__NAME__"},"ref":"v__VERSION__","dependencies":{"prelude":">=6.0.0 <7.0.0"}}"""

replace :: String -> String -> String -> String
replace pattern replacement =
  String.replaceAll (Pattern pattern) (Replacement replacement)
