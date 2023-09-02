module Aff.Invincible where

import Prelude

import Effect.Aff (Aff, Milliseconds(..), delay, error, forkAff, invincible, killFiber)
import Effect.Class.Console (log)

example :: Aff Unit
example = do
  a <- forkAff $ invincible do
    log "invincible fun start."
    delay (Milliseconds 200.0)
    log "invincible fun end."
  
  -- invincibleをかましてないと、endが表示される前に中断されてしまう
  _ <- killFiber (error "kill") a
  pure unit

