module Aff.Fork where

import Prelude

import Aff.Util (affLog)
import Data.Traversable (traverse)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, joinFiber)

example :: Aff (Fiber Unit)
example = forkAff do
  affLog "Parent Start"
  a <- forkAff do
    delay (Milliseconds 500.0)
    affLog "Call Child1"
  b <- forkAff do
    affLog "Call Child2"
  c <- forkAff do
    delay (Milliseconds 200.0)
    affLog "Call Child3"
  _ <- traverse joinFiber [a, b, c]
  affLog "Parent End"