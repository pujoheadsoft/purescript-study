module Aff.Supervise where

import Prelude

import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, supervise)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)

example :: Aff Unit
example = do
  log "Parent process start."
  supervise do
    _ <- forkAff do
      delay (Milliseconds 3000.0)
      liftEffect $ throw "cancelled" -- 親が終了するとキャンセルされるので時間がかかっているこいつは呼ばれない

    _ <- forkAff do
      delay (Milliseconds 10.0)
      log "child2" -- こいつは親より時間がかからないから呼ばれる

    delay (Milliseconds 100.0)
    log "done"

  log "Parent process end."
  
