module Aff.Delay where

import Prelude

import Aff.Util (affLog)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay)

example :: Aff Unit
example = do
  affLog "Before Delay"
  delay (Milliseconds 500.0)
  affLog "After Delay"
