module Aff.Delay where

import Prelude

import Aff.Util (affLog)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, Fiber, delay, forkAff)

example :: Aff (Fiber Unit)
example = forkAff do
  affLog "Before Delay"
  delay (Milliseconds 500.0)
  affLog "After Delay"
