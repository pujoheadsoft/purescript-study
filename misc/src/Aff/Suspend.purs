module Aff.Suspend where

import Prelude

import Aff.Util (affLog)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, joinFiber, suspendAff)

example :: Aff Unit
example = do
  -- これはすぐ実行される
  f1 <- forkAff do
    affLog "A"
  -- これはjoinFiberするまで実行されない(suspendされる)
  f2 <- suspendAff do
    affLog "B"
  affLog "C"
  delay (Milliseconds 300.0)
  affLog "D"
  joinFiber f1
  joinFiber f2