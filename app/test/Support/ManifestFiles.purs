module Test.Support.ManifestFiles (ManifestFiles, readFiles) where

import Registry.App.Prelude

import Node.FS.Aff as FS
import Node.Path as Path

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
fixtureFile file = Path.concat [ "test", "fixtures", "halogen-hooks", file ]

readLicense :: Aff String
readLicense = FS.readTextFile UTF8 $ fixtureFile "LICENSE"

readPackageJson :: Aff String
readPackageJson = FS.readTextFile UTF8 $ fixtureFile "package.json"

readSpagoDhall :: Aff String
readSpagoDhall = FS.readTextFile UTF8 $ fixtureFile "spago.dhall"

readBowerJson :: Aff String
readBowerJson = FS.readTextFile UTF8 $ fixtureFile "bower.json"
