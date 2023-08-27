module Aff.Apathize where

import Prelude

import Aff.Util (affLog)
import Effect.Aff (Aff, Fiber, apathize, forkAff)
import Effect.Class.Console (error)

example :: Aff (Fiber Unit)
example = forkAff do
  -- apathizeはattemptと異なり返ってくる型にErrorがない
  apathize $ forkAff do
    error "error"
  affLog "apathize"