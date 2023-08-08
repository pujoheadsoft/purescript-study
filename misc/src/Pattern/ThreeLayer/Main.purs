module Pattern.ThreeLayer.Main where

import Prelude

import Effect (Effect)
import Pattern.ThreeLayer.Layer1 (runApp)
import Pattern.ThreeLayer.Layer2 (program)

main :: Effect Unit
main = do
  let globalEnvironmentInfo = { someValue: 1000 }
  runApp program globalEnvironmentInfo
