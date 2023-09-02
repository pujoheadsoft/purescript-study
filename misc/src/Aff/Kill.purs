module Aff.Kill where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, makeAff, message, try)
import Effect.Class.Console (log)

example :: Aff Unit
example = do
  fiber <- forkAff do
    log "child start."
    -- killFiberするとCancelerが呼ばれる
    -- callbackを使わないでcancelerだけ返す
    _ <- makeAff \_ -> pure $ Canceler \_ -> do
      log "Cancelled."
      
    -- killされるとこれは呼ばれない
    log "child end."
  
  delay (Milliseconds 50.0)
  killFiber (error "Nope") fiber
  
  res <- try (joinFiber fiber)

  case (lmap message res) of
    Left m -> log m
    Right _ -> pure unit