module Main where

import Aff.MyAff as MyAff

import Prelude

import Aff.Main as AffMain
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, forkAff, launchAff_)
import Effect.Class.Console (log)
import Pattern.FourLayer.Main as FourLayer
import Pattern.ReaderT.ReaderT as RederTPattern
import Pattern.ThreeLayer.Main as ThreeLayer

main :: Effect Unit
main = do
  launchAff_ do
    _ <- forkAff do
      delay $ Milliseconds 100.0
      log "hoge"

    _ <- forkAff do
      log "moge"

    pure unit

  MyAff.launchAff_ do
    _ <- MyAff.forkAff do
      MyAff.delay $ Milliseconds 100.0
      log "foo"

    _ <- MyAff.forkAff do
      log "bar"

    pure unit

-- main :: Effect Unit
-- main = do
--   RederTPattern.main
--   ThreeLayer.main
--   FourLayer.main
--   AffMain.main
