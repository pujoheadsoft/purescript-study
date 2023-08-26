module Aff.Main where

import Prelude

import Aff.Bracket as Bracket
import Aff.Delay as Delay
import Aff.Fork as Fork
import Aff.Kill as Kill
import Aff.Supervise as Supervise
import Aff.Suspend as Suspend
import Aff.Util (affLog)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, joinFiber, launchAff_)

execute :: Aff (Fiber Unit) -> String -> Aff Unit
execute a title = do
  affLog $ "\n[" <> title <> " Example Start]"
  joinFiber =<< a
  affLog $ "[" <> title <> " Example End]"

main :: Effect Unit
main = launchAff_ do
  execute Delay.example "Delay"
  execute Fork.example "Fork"
  execute Suspend.example "Suspend"
  execute Bracket.example "Bracket"
  execute Supervise.example "Supervise"
  execute Kill.example "Kill"