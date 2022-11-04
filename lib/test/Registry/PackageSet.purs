module Test.Registry.PackageSet (spec) where

import Prelude

import Registry.PackageSet as PackageSet
import Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Round-trips package set fixtures" do
    Assert.shouldRoundTrip "PackageSet" PackageSet.codec
      [ { label: "0.0.1", value: initialPackageSet }
      ]

-- A trimmed version of the first package set release
initialPackageSet :: String
initialPackageSet =
  """
{
  "version": "0.0.1",
  "compiler": "0.15.4",
  "published": "2022-09-24",
  "packages": {
    "ace": "9.0.0",
    "aff": "7.1.0",
    "aff-bus": "6.0.0",
    "aff-coroutines": "9.0.0",
    "aff-promise": "4.0.0",
    "aff-retry": "2.0.0",
    "affjax": "13.0.0",
    "affjax-node": "1.0.0",
    "affjax-web": "1.0.0",
    "ansi": "7.0.0",
    "argonaut": "9.0.0",
    "argonaut-codecs": "9.1.0",
    "argonaut-core": "7.0.0",
    "argonaut-generic": "8.0.0",
    "argonaut-traversals": "10.0.0",
    "argparse-basic": "2.0.0"
  }
}
"""
