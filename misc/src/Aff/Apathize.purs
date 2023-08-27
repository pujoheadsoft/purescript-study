module Aff.Apathize where

import Prelude

import Aff.Util (affLog)
import Effect.Aff (Aff, apathize, forkAff)
import Effect.Class.Console (error)

example :: Aff Unit
example = do
  -- apathizeはattemptと異なり返ってくる型にErrorがない
  apathize $ forkAff do
    error "error"
  affLog "apathize"