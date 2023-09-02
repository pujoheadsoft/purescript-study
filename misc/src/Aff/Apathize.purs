module Aff.Apathize where

import Prelude

import Effect.Aff (Aff, apathize, forkAff)
import Effect.Class.Console (error, log)

example :: Aff Unit
example = do
  -- apathizeはattemptと異なり返ってくる型にErrorがない
  apathize $ forkAff do
    error "error"
  log "apathize"