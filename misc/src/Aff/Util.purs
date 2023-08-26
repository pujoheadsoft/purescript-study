module Aff.Util where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

affLog :: String -> Aff Unit
affLog = liftEffect <<< log 