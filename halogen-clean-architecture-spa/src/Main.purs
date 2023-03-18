module Main where

import Prelude

import Component.Router as Router
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Router.component unit body

  -- runUI Button.component unit body
  -- void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
  --   when (old /= Just new) do
  --     launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate
