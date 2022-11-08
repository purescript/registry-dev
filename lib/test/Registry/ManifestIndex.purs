module Test.Registry.ManifestIndex (spec) where

import Prelude

import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Registry.ManifestIndex as ManifestIndex
import Test.Assert as Assert
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.it "Round-trips package entry fixture" do
    let contextEntry' = String.trim contextEntry
    let parsedContext = ManifestIndex.parseEntry contextEntry'
    contextEntry' `Assert.shouldEqualRight` map (ManifestIndex.printEntry <<< NonEmptySet.fromFoldable1) parsedContext

contextEntry :: String
contextEntry =
  """
{"name":"context","version":"0.0.1","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
{"name":"context","version":"0.0.2","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
{"name":"context","version":"0.0.3","license":"MIT","location":{"githubOwner":"Fresheyeball","githubRepo":"purescript-owner"},"dependencies":{}}
"""
