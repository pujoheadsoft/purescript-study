module Aff.Main where

import Prelude

import Aff.Bracket as Bracket
import Aff.Delay as Delay
import Aff.Fork as Fork
import Aff.Suspend as Suspend
import Effect (Effect)
import Effect.Aff (Aff, Fiber, joinFiber, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

execute :: Aff (Fiber Unit) -> String -> Aff Unit
execute a title = do
  liftEffect $ log $ "\n[" <> title <> " Example Start]"
  joinFiber =<< a
  liftEffect $ log $ "[" <> title <> " Example End]"

main :: Effect Unit
main = launchAff_ do
  execute Delay.example "Delay"
  execute Fork.example "Fork"
  execute Suspend.example "Suspend"
  execute Bracket.example "Bracket"
