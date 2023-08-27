module Aff.Finally where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, error, finally, makeAff)
import Effect.Console (log)

example :: Aff Unit
example = do
  let
    a1 = makeAff \callback -> do
      log "call 1st fn"
      callback (Left (error "error"))
      pure mempty
      
    a2 = makeAff \callback -> do
      log "call 2nd fn"
      callback (Right unit)
      pure mempty

  _ <- finally a1 a2
  pure unit

{-
finally :: forall a. Aff Unit -> Aff a -> Aff a
finally fin a = 
  bracket 
    (pure unit) -- 準備
    (const fin) -- 後処理
    (const a)   -- 実行

-}