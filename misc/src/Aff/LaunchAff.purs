module Aff.LaunchAff where

import Prelude

import Effect (Effect)
import Effect.Aff (joinFiber, launchAff, launchAff_, launchSuspendedAff)
import Effect.Class.Console (log)

example :: Effect Unit
example = do
  -- joinFiberするまえsuspendされる
  suspendedFiber <- launchSuspendedAff do
    log "launchSuspendedAff"
    pure "launchSuspendedAff result"

  -- 実行されて返り値を使える
  fiber <- launchAff do
    log "launchAff"
    pure "launchAff result"
  
  -- 実行されるが返り値はない
  launchAff_ do
    log "launchAff_"
  
  -- affの結果を出力
  launchAff_ do
    log =<< joinFiber fiber
  
  -- suspendされていたfiberを実行
  launchAff_ do
    log =<< joinFiber suspendedFiber
  
