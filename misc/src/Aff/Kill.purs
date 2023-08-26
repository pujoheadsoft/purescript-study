module Aff.Kill where

import Prelude

import Aff.Util (affLog)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff (Aff, Canceler(..), Fiber, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, makeAff, message, try)

example :: Aff (Fiber Unit)
example = forkAff do
  fiber <- forkAff do
    _ <- makeAff \_ -> pure $ Canceler \_ -> do
      affLog "Cancelled."
    affLog "Call child."
  
  delay (Milliseconds 50.0)
  killFiber (error "Nope") fiber
  
  res <- try (joinFiber fiber)

  case (lmap message res) of
    Left m -> affLog m
    Right _ -> pure unit