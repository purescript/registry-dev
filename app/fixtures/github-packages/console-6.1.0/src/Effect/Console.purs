-- | This module provides functions for outputting strings to the console.
module Effect.Console
  ( log
  , logShow
  , warn
  , warnShow
  , error
  , errorShow
  , info
  , infoShow
  , debug
  , debugShow
  , time
  , timeLog
  , timeEnd
  , clear
  ) where

import Prelude

import Effect (Effect)

foreign import log :: String -> Effect Unit
foreign import warn :: String -> Effect Unit
foreign import error :: String -> Effect Unit
foreign import info :: String -> Effect Unit
foreign import debug :: String -> Effect Unit
foreign import time :: String -> Effect Unit
foreign import timeLog :: String -> Effect Unit
foreign import timeEnd :: String -> Effect Unit
foreign import clear :: Effect Unit

logShow :: forall a. Show a => a -> Effect Unit
logShow = log <<< show

warnShow :: forall a. Show a => a -> Effect Unit
warnShow = warn <<< show

errorShow :: forall a. Show a => a -> Effect Unit
errorShow = error <<< show

infoShow :: forall a. Show a => a -> Effect Unit
infoShow = info <<< show

debugShow :: forall a. Show a => a -> Effect Unit
debugShow = debug <<< show
