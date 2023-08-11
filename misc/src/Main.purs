module Main where

import Prelude

import Effect (Effect)
import Aff.Main as AffMain
import Pattern.FourLayer.Main as FourLayer
import Pattern.ReaderT.ReaderT as RederTPattern
import Pattern.ThreeLayer.Main as ThreeLayer

main :: Effect Unit
main = do
  RederTPattern.main
  ThreeLayer.main
  FourLayer.main
  AffMain.main
