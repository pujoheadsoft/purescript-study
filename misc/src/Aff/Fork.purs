module Aff.Fork where

import Prelude

import Data.Traversable (traverse)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, joinFiber)
import Effect.Class (liftEffect)
import Effect.Console (log)

example :: Aff (Fiber Unit)
example = forkAff do
  liftEffect $ log "Parent Start"
  a <- forkAff do
    delay (Milliseconds 500.0)
    liftEffect $ log "Call Child1"
  b <- forkAff do
    liftEffect $ log "Call Child2"
  c <- forkAff do
    delay (Milliseconds 200.0)
    liftEffect $ log "Call Child3"
  _ <- traverse joinFiber [a, b, c]
  liftEffect $ log "Parent End"