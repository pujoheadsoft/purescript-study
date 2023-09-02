module Aff.RunAff where

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Aff (joinFiber, launchAff_, message, runAff, runAff_, runSuspendedAff)
import Effect.Class.Console (log)

example :: Effect Unit
example = do
  -- joinFiberするまえsuspendされる
  suspendedFiber <- runSuspendedAff
    (either (log <<< message) log) -- Left(エラー)でもRight(成功)でもログ出力
    do 
      log "runSuspendedAff"
      pure "runSuspendedAff result"

  -- 実行されて返り値を使える
  fiber <- runAff
    (either (log <<< message) log)
    do
      log "runAff"
      pure "runAff result"
  
  -- 実行されるが返り値はない
  runAff_
    (either (log <<< message) log)
    do
      log "runAff_"
      pure "runAff_ result"
  
  launchAff_ $ joinFiber fiber
  
  launchAff_ $ joinFiber suspendedFiber
  
  pure unit