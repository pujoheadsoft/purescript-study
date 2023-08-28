module Aff.Finally where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, error, finally, makeAff, try)
import Effect.Console (log)

example :: Aff Unit
example = do
  let
    finalizer = makeAff \callback -> do
      log "finalize"
      callback (Right unit)
      pure mempty
      
    a = makeAff \callback -> do
      log "call failed fn"
      callback (Left $ error "fail")
      pure mempty
  
  -- 次のようにした場合、aの実行でエラーになり処理は止まりfinalizerは呼ばれないが、finallyを使えばfinalizerは絶対に呼ばれる
  -- _ <- joinFiber =<< forkAff a
  -- _ <- joinFiber =<< forkAff finalizer
  
  _ <- try $ finally finalizer a
  
  pure unit
