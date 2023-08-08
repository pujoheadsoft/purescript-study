module Main where

import Prelude

import Effect (Effect)
import Pattern.ReaderT as RederTPattern
import Pattern.ThreeLayer.Main as ThreeLayer
import Pattern.FourLayer.Main as FourLayer

main :: Effect Unit
main = do
  RederTPattern.main
  ThreeLayer.main
  FourLayer.main
