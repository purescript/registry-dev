module Test.Registry.Manifest (spec) where

import Prelude

import Data.String as String
import Data.Traversable (for)
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
