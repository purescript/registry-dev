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

  Spec.it "Decodes historical manifests with deprecated SPDX identifiers" do
    for_ historicalManifests \{ label, input } ->
      case JSON.parse input of
        Left err ->
          Assert.fail $ "Failed to parse test JSON for " <> label <> ": " <> err
        Right json ->
          case CJ.decode Manifest.codec json of
            Left err ->
              Assert.fail $ "Failed to decode historical manifest for " <> label <> ": " <> CJ.DecodeError.print err
            Right _ ->
              pure unit

historicalManifests :: Array { label :: String, input :: String }
historicalManifests =
  [ { label: "AGPL-3.0", input: historicalManifest "jarilo" "1.0.1" "AGPL-3.0" }
  , { label: "LGPL-3.0", input: historicalManifest "matrices" "5.0.0" "LGPL-3.0" }
  , { label: "LGPL-3.0+", input: historicalManifest "test-unit" "17.0.0" "LGPL-3.0+" }
  , { label: "GPL-3.0 AND MIT", input: historicalManifest "nano-id" "1.1.0" "GPL-3.0 AND MIT" }
  , { label: "LGPL-2.1 AND LGPL-2.1-only", input: historicalManifest "bookhound" "0.1.1" "LGPL-2.1 AND LGPL-2.1-only" }
  ]

historicalManifest :: String -> String -> String -> String
historicalManifest name version license =
  historicalManifestTemplate
    # replace "__LICENSE__" license
    # replace "__VERSION__" version
    # replace "__NAME__" name

historicalManifestTemplate :: String
historicalManifestTemplate =
  """{"name":"__NAME__","version":"__VERSION__","license":"__LICENSE__","location":{"githubOwner":"purescript","githubRepo":"purescript-__NAME__"},"ref":"v__VERSION__","dependencies":{"prelude":">=6.0.0 <7.0.0"}}"""

replace :: String -> String -> String -> String
replace pattern replacement =
  String.replaceAll (Pattern pattern) (Replacement replacement)
