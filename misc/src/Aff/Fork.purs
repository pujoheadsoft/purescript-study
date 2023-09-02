module Aff.Fork where

import Prelude

import Data.Traversable (traverse)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, joinFiber)
import Effect.Class.Console (log)

example :: Aff Unit
example = do
  log "Parent Start"
  a <- forkAff do
    delay (Milliseconds 500.0)
    log "Call Child1"
  b <- forkAff do
    log "Call Child2"
  c <- forkAff do
    delay (Milliseconds 200.0)
    log "Call Child3"
  _ <- traverse joinFiber [a, b, c]
  log "Parent End"