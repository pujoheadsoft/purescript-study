module Main where

import Prelude

import Aff.Main as AffMain
import Aff.MyAff as MyAff
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, forkAff, joinFiber, launchAff_)
import Effect.Class.Console (log)
import Pattern.FourLayer.Main as FourLayer
import Pattern.ReaderT.ReaderT as RederTPattern
import Pattern.ThreeLayer.Main as ThreeLayer

main :: Effect Unit
main = do
  launchAff_ do
    a <- forkAff do
      delay $ Milliseconds 100.0
      log "hoge"
      pure "a"

    b <- forkAff do
      log "moge"
      pure "b"

    log =<< joinFiber a
    log =<< joinFiber b

  MyAff.launchAff_ do
    a <- MyAff.forkAff do
      MyAff.delay $ Milliseconds 100.0
      log "hoge"
      pure "a"

    b <- MyAff.forkAff do
      log "moge"
      pure "b"

    log =<< MyAff.joinFiber a
    log =<< MyAff.joinFiber b

-- main :: Effect Unit
-- main = do
--   RederTPattern.main
--   ThreeLayer.main
--   FourLayer.main
--   AffMain.main
