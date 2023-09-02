module Aff.Suspend where

import Prelude

import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, joinFiber, suspendAff)
import Effect.Class.Console (log)

example :: Aff Unit
example = do
  -- これはすぐ実行される
  f1 <- forkAff do
    log "A"
  -- これはjoinFiberするまで実行されない(suspendされる)
  f2 <- suspendAff do
    log "B"
  log "C"
  delay (Milliseconds 300.0)
  log "D"
  joinFiber f1
  joinFiber f2