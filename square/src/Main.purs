module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Math (sqrt)
import Data.Number.Format

diagonal w h = sqrt(w * w + h * h)

main :: Effect Unit
main = do
  log(toString(diagonal 3.0 4.0))
