module Main where

import Prelude

import Aff.Main as AffMain
import Effect (Effect)
import Pattern.FourLayer.Main as FourLayer
import Pattern.ReaderT.ReaderT as RederTPattern
import Pattern.ThreeLayer.Main as ThreeLayer
import TaglessFinal.Main as TaglessFinal

main :: Effect Unit
main = do
  RederTPattern.main
  ThreeLayer.main
  FourLayer.main
  AffMain.main
  TaglessFinal.main
