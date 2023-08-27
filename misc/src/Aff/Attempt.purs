module Aff.Attempt where

import Prelude

import Aff.Util (affLog)
import Data.Either (Either(..))
import Effect.Aff (Aff, Fiber, attempt, forkAff, message)
import Effect.Class.Console (error)

example :: Aff (Fiber Unit)
example = forkAff do
  -- attemptはtryと書くのと同じ
  v <- attempt $ forkAff do
    error "error"
    pure "unit"
  case v of
    Left e -> affLog $ message e
    Right _ -> pure unit
