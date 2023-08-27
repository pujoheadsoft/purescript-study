module Aff.Kill where

import Prelude

import Aff.Util (affLog)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, makeAff, message, try)

example :: Aff Unit
example = do
  fiber <- forkAff do
    affLog "child start."
    -- killFiberするとCancelerが呼ばれる
    -- callbackを使わないでcancelerだけ返す
    _ <- makeAff \_ -> pure $ Canceler \_ -> do
      affLog "Cancelled."
      
    -- killされるとこれは呼ばれない
    affLog "child end."
  
  delay (Milliseconds 50.0)
  killFiber (error "Nope") fiber
  
  res <- try (joinFiber fiber)

  case (lmap message res) of
    Left m -> affLog m
    Right _ -> pure unit