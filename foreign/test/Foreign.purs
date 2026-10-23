module Test.Foreign (main) where

import Prelude

import Effect (Effect)
import Test.Registry.Foreign.FastGlob as Foreign.FastGlob
import Test.Registry.Foreign.Gzip as Foreign.Gzip
import Test.Registry.Foreign.JsonRepair as Foreign.JsonRepair
import Test.Registry.Foreign.SPDX as Foreign.SPDX
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Spec.describe "Foreign" do
    Spec.describe "SPDX" Foreign.SPDX.spec
    Spec.describe "JsonRepair" Foreign.JsonRepair.spec
    Spec.describe "FastGlob" Foreign.FastGlob.spec
    Spec.describe "Gzip" Foreign.Gzip.spec
