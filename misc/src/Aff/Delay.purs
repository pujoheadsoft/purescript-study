module Aff.Delay where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Console (log)

main :: Aff Unit
main = do
  liftEffect $ log "Before Delay"
  delay (Milliseconds 500.0)
  liftEffect $ log "After Delay"