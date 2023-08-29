module Aff.LaunchAff where

import Prelude

import Aff.Util (affLog)
import Effect (Effect)
import Effect.Aff (joinFiber, launchAff, launchAff_, launchSuspendedAff)

example :: Effect Unit
example = do
  -- joinFiberするまえsuspendされる
  suspendedFiber <- launchSuspendedAff do
    affLog "launchSuspendedAff"
    pure "launchSuspendedAff result"

  -- 実行されて返り値を使える
  fiber <- launchAff do
    affLog "launchAff"
    pure "launchAff result"
  
  -- 実行されるが返り値はない
  launchAff_ do
    affLog "launchAff_"
  
  -- affの結果を出力
  launchAff_ do
    affLog =<< joinFiber fiber
  
  -- suspendされていたfiberを実行
  launchAff_ do
    affLog =<< joinFiber suspendedFiber
  
