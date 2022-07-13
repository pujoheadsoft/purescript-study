module Main where

import Data.Number.Format (toString)
import Prelude

import Data.Number (sqrt)
import Effect (Effect)
import Effect.Console (log)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt(w * w + h * h)

main :: Effect Unit
main = do
  log(toString(diagonal 3.0 4.0))
