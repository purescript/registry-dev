module Test.Foreign (main) where

import Registry.App.Prelude

import Test.Registry.Foreign.JsonRepair as Foreign.JsonRepair
import Test.Registry.Foreign.SPDX as Foreign.SPDX
import Test.Registry.Foreign.Tar as Foreign.Tar
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = launchAff_ $ runSpec' defaultConfig [ consoleReporter ] do
  Spec.describe "Foreign" do
    Spec.describe "Tar" Foreign.Tar.spec
    Spec.describe "SPDX" Foreign.SPDX.spec
    Spec.describe "JsonRepair" Foreign.JsonRepair.spec
