module Test.Registry.App.CLI.PursVersions (spec) where

import Registry.App.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Registry.App.CLI.PursVersions (pursVersions)
import Registry.Test.Assert as Assert
import Registry.Test.Assert.Run as Test.Run
import Registry.Version as Version
import Test.Spec as Spec

-- NOTE: This should be kept up to date as new versions of the compiler are released.
-- Upon release of a new compiler version and update of `purescript-overlay`, the tests below will fail until the version is added here.
knownCompilers :: Array Version
knownCompilers = map (unsafeFromRight <<< Version.parse)
  [ "0.13.0"
  , "0.13.2"
  , "0.13.3"
  , "0.13.4"
  , "0.13.5"
  , "0.13.6"
  , "0.13.8"
  , "0.14.0"
  , "0.14.1"
  , "0.14.2"
  , "0.14.3"
  , "0.14.4"
  , "0.14.5"
  , "0.14.6"
  , "0.14.7"
  , "0.14.8"
  , "0.14.9"
  , "0.15.0"
  , "0.15.2"
  , "0.15.3"
  , "0.15.4"
  , "0.15.5"
  , "0.15.6"
  , "0.15.7"
  , "0.15.8"
  , "0.15.9"
  , "0.15.10"
  , "0.15.11"
  , "0.15.12"
  , "0.15.13"
  , "0.15.14"
  ]

spec :: Spec.Spec Unit
spec = do
  Spec.it "All expected versions are present in purs-version" do
    versionsNonEmpty <- Test.Run.runBaseEffects pursVersions
    let versions = NEA.toArray versionsNonEmpty
    Array.sort versions `Assert.shouldEqual` Array.sort knownCompilers
