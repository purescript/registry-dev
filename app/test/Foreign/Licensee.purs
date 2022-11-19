module Test.Foreign.Licensee where

import Registry.App.Prelude

import Foreign.Licensee as Licensee
import Node.Path as Path
import Test.Spec as Spec
import Test.Support.ManifestFiles as ManifestFiles
import Test.Utils.Assert as Assert

licensee :: Spec.Spec Unit
licensee = do
  let fixtures = Path.concat [ "test", "fixtures", "halogen-hooks" ]
  Spec.describe "Licensee runs" do
    Spec.it "Detects from directory" do
      detected <- Licensee.detect fixtures
      Assert.shouldEqual (Right [ "MIT", "Apache-2.0" ]) detected

    Spec.it "Detects from file list in memory" do
      files <- ManifestFiles.readFiles
      detected <- Licensee.detectFiles
        [ { name: "package.json", contents: files.packageJson }
        , { name: "LICENSE", contents: files.license }
        ]
      Assert.shouldEqual (Right [ "MIT", "Apache-2.0" ]) detected

    Spec.it "Returns empty array when no license can be found" do
      detected <- Licensee.detectFiles []
      Assert.shouldEqual (Right []) detected

    Spec.it "Detects from package.json only" do
      files <- ManifestFiles.readFiles
      detected <- Licensee.detectFiles [ { name: "package.json", contents: files.packageJson } ]
      Assert.shouldEqual (Right [ "Apache-2.0" ]) detected

    Spec.it "Detects from LICENSE only" do
      files <- ManifestFiles.readFiles
      detected <- Licensee.detectFiles [ { name: "LICENSE", contents: files.license } ]
      Assert.shouldEqual (Right [ "MIT" ]) detected

    Spec.it "Doesn't detect from Bower or Spago" do
      files <- ManifestFiles.readFiles
      detected <- Licensee.detectFiles
        [ { name: "bower.json", contents: files.bowerJson }
        , { name: "spago.dhall", contents: files.spagoDhall }
        ]
      Assert.shouldEqual (Right []) detected
