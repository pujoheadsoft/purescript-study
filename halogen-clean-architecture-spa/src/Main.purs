module Main where

import Prelude

import Component.Router as Router
import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Test.Mock as M

main :: Effect Unit
main = M.main

-- main :: Effect Unit
-- main = HA.runHalogenAff do
--   body <- HA.awaitBody
--   runUI Router.component unit body