module Foreign.JsonRepair (tryRepair, repair) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..), fromMaybe)

foreign import repairImpl :: forall r. Fn3 r (String -> r) String r

repair :: String -> Maybe String
repair = runFn3 repairImpl Nothing Just

tryRepair :: String -> String
tryRepair input = fromMaybe input $ repair input
