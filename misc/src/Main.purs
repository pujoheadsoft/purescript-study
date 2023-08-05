module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Pattern.ReaderT as RederTPattern

{-
  似た関数として
  view
  review
  preview
  がある
-}

main :: Effect Unit
main = do
  RederTPattern.main
  log "🍝"
