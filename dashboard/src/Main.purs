module Dashboard.Main where

import Prelude

import Dashboard.Router as Router
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  initialRoute <- liftEffect Router.getRouteFromHash
  runUI Router.component initialRoute body
