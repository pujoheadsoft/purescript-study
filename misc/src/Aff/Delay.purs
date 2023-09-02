module Aff.Delay where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect.Class.Console (log)

example :: Aff Unit
example = do
  log "Before Delay"
  delay (Milliseconds 500.0)
  log "After Delay"
