module Aff.Main where

import Prelude

import Aff.Apathize as Apathize
import Aff.Attempt as Attempt
import Aff.Bracket as Bracket
import Aff.Delay as Delay
import Aff.Finally as Finally
import Aff.Fork as Fork
import Aff.Kill as Kill
import Aff.MakeAff as MakeAff
import Aff.Supervise as Supervise
import Aff.Suspend as Suspend
import Aff.Util (affLog)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)

execute :: Aff Unit -> String -> Aff Unit
execute a title = do
  affLog $ "\n[" <> title <> " example start]"
  a
  affLog $ "[" <> title <> " example end]"

main :: Effect Unit
main = launchAff_ do
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
