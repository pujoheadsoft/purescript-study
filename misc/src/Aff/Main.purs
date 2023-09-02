module Aff.Main where

import Prelude

import Aff.Apathize as Apathize
import Aff.Attempt as Attempt
import Aff.Bracket as Bracket
import Aff.Delay as Delay
import Aff.Finally as Finally
import Aff.Fork as Fork
import Aff.Invincible as Invincible
import Aff.Kill as Kill
import Aff.LaunchAff as LaunchAff
import Aff.MakeAff as MakeAff
import Aff.RunAff as RunAff
import Aff.Supervise as Supervise
import Aff.Suspend as Suspend
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)

execute :: Aff Unit -> String -> Aff Unit
execute a title = do
  log $ "\n[" <> title <> " example start]"
  a
  log $ "[" <> title <> " example end]"

executeEff :: Effect Unit -> String -> Effect Unit
executeEff a title = do
  log $ "\n[" <> title <> " example start]"
  a
  log $ "[" <> title <> " example end]"

main :: Effect Unit
main = do
  executeEff LaunchAff.example "launchAff"
  executeEff RunAff.example "runAff"
  launchAff_ do
    execute Delay.example "delay"
    execute Fork.example "forkAff"
    execute Suspend.example "suspend"
    execute Bracket.example "bracket"
    execute Supervise.example "supervise"
    execute Kill.example "killFiber"
    execute Attempt.example "attempt"
    execute Apathize.example "apathize"
    execute Finally.example "finally"
    execute MakeAff.example "makeAff"
    execute Invincible.example "invincible"

