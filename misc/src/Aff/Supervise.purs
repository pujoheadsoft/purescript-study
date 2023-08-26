module Aff.Supervise where

import Prelude

import Aff.Util (affLog)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, supervise)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

example :: Aff (Fiber Unit)
example = forkAff do
  affLog "Parent process start."
  supervise do
    _ <- forkAff do
      delay (Milliseconds 3000.0)
      liftEffect $ throw "cancelled" -- 親が終了するとキャンセルされるので時間がかかっているこいつは呼ばれない

    _ <- forkAff do
      delay (Milliseconds 10.0)
      affLog "child2" -- こいつは親より時間がかからないから呼ばれる

    delay (Milliseconds 100.0)
    affLog "done"

  affLog "Parent process end."
  
