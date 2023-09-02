module Aff.MakeAff where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, Canceler(..), error, forkAff, killFiber, makeAff, message, nonCanceler)
import Effect.Class.Console (log)

example :: Aff Unit
example = do
  fiber1 <- forkAff do
    a <- makeAff \callback -> do
      log "use effect 1."
      callback (Right "Done 1")
      pure nonCanceler -- 何もしないcancelerを返す。 pure mempty と書いてもOK (mempty = nonCanceler だから)
    log a -- killされなければ Done 1

  killFiber (error "cancel") fiber1
  
  fiber2 <- forkAff do
    b <- makeAff \callback -> do
      log "use effect 2."
      callback (Right "Done 2")
      -- ログ出力するcanceler
      pure $ Canceler \e ->
        log $ message e 
    log b -- killされなければ Done 2

  killFiber (error "cancel") fiber2
  
  pure unit
  
