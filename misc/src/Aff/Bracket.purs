module Aff.Bracket where

import Prelude

import Effect.Aff (Aff, Milliseconds(..), bracket, delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref

example :: Aff Unit
example = do
  ref <- liftEffect $ Ref.new ""
  let
    action s = do
      delay (Milliseconds 100.0)
      _ <- liftEffect $ Ref.modify (_ <> s) ref
      pure s
  _ <- bracket
    (action "1")                      -- リソースの獲得
    (\s -> void $ action (s <> "3"))  -- リソースの処分(最後に呼ばれる)
    (\s -> action (s <> "2"))         -- リソースの使用

  value <- liftEffect $ Ref.read ref
  
  liftEffect $ log value
  
