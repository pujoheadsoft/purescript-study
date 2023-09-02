module Aff.Attempt where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, attempt, forkAff, message)
import Effect.Class.Console (error, log)

example :: Aff Unit
example = do
  -- attemptはtryと書くのと同じ
  v <- attempt $ forkAff do
    error "error"
    pure "unit"
  case v of
    Left e -> log $ message e
    Right _ -> pure unit
