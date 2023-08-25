module Aff.Suspend where

import Prelude

import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, joinFiber, suspendAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

example :: Aff (Fiber Unit)
example = forkAff do
  -- これはすぐ実行される
  f1 <- forkAff do
    liftEffect $ log "A"
  -- これはjoinFiberするまで実行されない(suspendされる)
  f2 <- suspendAff do
    liftEffect $ log "B"
  liftEffect $ log "C"
  delay (Milliseconds 300.0)
  liftEffect $ log "D"
  joinFiber f1
  joinFiber f2