module Test.Registry.App.CLI.Licensee (spec) where

import Registry.App.Prelude

import Node.FS.Aff as FS.Aff
import Node.Path as Path
import Registry.App.CLI.Licensee as Licensee
import Registry.Test.Assert as Assert
import Test.Spec as Spec

spec :: Spec.Spec Unit
spec = do
  let fixtures = Path.concat [ "app", "fixtures", "manifest-files", "halogen-hooks" ]

  Spec.it "Detects from directory" do
    detected <- Licensee.detect fixtures
    Assert.shouldEqual (Right [ "MIT", "Apache-2.0" ]) detected

  Spec.it "Detects from file list in memory" do
    files <- readFiles
    detected <- Licensee.detectFiles
      [ { name: "package.json", contents: files.packageJson }
      , { name: "LICENSE", contents: files.license }
      ]
    Assert.shouldEqual (Right [ "MIT", "Apache-2.0" ]) detected

  Spec.it "Returns empty array when no license can be found" do
    detected <- Licensee.detectFiles []
    Assert.shouldEqual (Right []) detected

  Spec.it "Detects from package.json only" do
    files <- readFiles
    detected <- Licensee.detectFiles [ { name: "package.json", contents: files.packageJson } ]
    Assert.shouldEqual (Right [ "Apache-2.0" ]) detected

  Spec.it "Detects from LICENSE only" do
    files <- readFiles
    detected <- Licensee.detectFiles [ { name: "LICENSE", contents: files.license } ]
    Assert.shouldEqual (Right [ "MIT" ]) detected

  Spec.it "Doesn't detect from Bower or Spago" do
    files <- readFiles
    detected <- Licensee.detectFiles
      [ { name: "bower.json", contents: files.bowerJson }
      , { name: "spago.dhall", contents: files.spagoDhall }
      ]
    Assert.shouldEqual (Right []) detected

type ManifestFiles =
  { license :: String
  , packageJson :: String
  , spagoDhall :: String
  , bowerJson :: String
  }

readFiles :: Aff ManifestFiles
readFiles = do
  license <- readLicense
  packageJson <- readPackageJson
  spagoDhall <- readSpagoDhall
  bowerJson <- readBowerJson
  pure { license, packageJson, spagoDhall, bowerJson }

fixtureFile :: FilePath -> FilePath
fixtureFile file = Path.concat [ "app", "fixtures", "manifest-files", "halogen-hooks", file ]

readLicense :: Aff String
readLicense = FS.Aff.readTextFile UTF8 $ fixtureFile "LICENSE"

readPackageJson :: Aff String
readPackageJson = FS.Aff.readTextFile UTF8 $ fixtureFile "package.json"

readSpagoDhall :: Aff String
readSpagoDhall = FS.Aff.readTextFile UTF8 $ fixtureFile "spago.dhall"

readBowerJson :: Aff String
readBowerJson = FS.Aff.readTextFile UTF8 $ fixtureFile "bower.json"
