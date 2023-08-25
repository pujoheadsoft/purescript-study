module Aff.Main where

import Prelude

import Aff.Delay as Delay
import Aff.Fork as Fork
import Effect (Effect)
import Effect.Aff (Aff, forkAff, joinFiber, launchAff, launchAff_, runAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

execute :: Aff Unit -> String -> Aff Unit
execute a title = do
  liftEffect $ log $ "\n[" <> title <> " Example Start]"
  a
  liftEffect $ log $ "[" <> title <> " Example End]"

main :: Effect Unit
main = launchAff_ do
  fiber <- forkAff $ execute Delay.main "Delay"
  joinFiber fiber
  execute Fork.main "Fork"