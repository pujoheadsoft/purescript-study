module Pattern.FourLayer.Main where

import Prelude

import Effect (Effect)
import Pattern.FourLayer.APIWithInfrastructure (runApp)
import Pattern.FourLayer.Domain (program)

main :: Effect Unit
main = do
  let globalEnvironmentInfo = { someValue: 100 }
  runApp program globalEnvironmentInfo
