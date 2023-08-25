module Aff.Delay where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, Fiber, delay, forkAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

example :: Aff (Fiber Unit)
example = forkAff do
  liftEffect $ log "Before Delay"
  delay (Milliseconds 500.0)
  liftEffect $ log "After Delay"
