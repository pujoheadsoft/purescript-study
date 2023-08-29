module Aff.Invincible where

import Prelude

import Aff.Util (affLog)
import Effect.Aff (Aff, Milliseconds(..), delay, error, forkAff, invincible, killFiber)

example :: Aff Unit
example = do
  a <- forkAff $ invincible do
    affLog "invincible fun start."
    delay (Milliseconds 200.0)
    affLog "invincible fun end."
  
  -- invincibleをかましてないと、endが表示される前に中断されてしまう
  _ <- killFiber (error "kill") a
  pure unit

