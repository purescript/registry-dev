module Test.Registry.PackageIndex (spec) where

import Prelude

import Data.String as String
import Registry.PackageIndex as PackageIndex
import Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Round-trips package entry fixture" do
    let contextEntry' = String.trim contextEntry
    let parsedContext = PackageIndex.parseEntry contextEntry'
    contextEntry' `Assert.shouldEqualRight` map PackageIndex.printEntry parsedContext

contextEntry :: String
contextEntry =
  """
{"name":"context","version":"0.0.1","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
{"name":"context","version":"0.0.2","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
{"name":"context","version":"0.0.3","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
"""
