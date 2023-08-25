module Aff.Fork where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

main :: Aff Unit
main = do
  liftEffect $ log "Parent Start"
  _ <- forkAff do
    delay (Milliseconds 100.0)
    liftEffect $ log "Call Child1"
  _ <- forkAff do
    liftEffect $ log "Call Child2"
  liftEffect $ log "Parent End"