module Main where

import Aff.MyAff
import Prelude

import Aff.Main as AffMain
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Pattern.FourLayer.Main as FourLayer
import Pattern.ReaderT.ReaderT as RederTPattern
import Pattern.ThreeLayer.Main as ThreeLayer

main :: Effect Unit
main = launchAff_ do
  forkAff do
    delay $ Milliseconds 100.0
    log "hoge"

  forkAff do
    log "moge"

  pure unit

-- main :: Effect Unit
-- main = do
--   RederTPattern.main
--   ThreeLayer.main
--   FourLayer.main
--   AffMain.main
