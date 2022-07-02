module Example.Run.ExampleOnPursuit where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Effect (Effect)
import Effect.Class.Console (log)
import Study.Control.Monad.Run.Except (EXCEPT, catch, throw)
import Study.Control.Monad.Run.Run (EFFECT, Run, liftEffect, runBaseEffect)
import Study.Control.Monad.Run.State (STATE, gets, modify, runState)
import Type.Row (type (+))

{-
  purescript-runのPursuitに載ってる例
  https://pursuit.purescript.org/packages/purescript-run/0.4.0/docs/Run
-}

type MyEffects = ( STATE Int + EXCEPT String + EFFECT + () )

yesProgram :: Run MyEffects Unit
yesProgram = do
  log("yesProgram Start ---------------")
  whenM (gets (_ < 0)) do
    throw "Number is less than 0"
  whileM_ (gets (_ > 0)) do
    x <- gets show
    liftEffect $ log $ "Yes: " <> x
    modify (_ - 1)
  log("yesProgram End -----------------")
  where
  whileM_
    :: forall a
     . Run MyEffects Boolean
    -> Run MyEffects a
    -> Run MyEffects Unit
  whileM_ mb ma = flip tailRecM unit \_ ->
    -- mbがtrueだったらloop、falseだったら終了
    mb >>= if _ then ma $> Loop unit else pure $ Done unit
  
main :: Effect Unit
main =
  yesProgram
    # catch (liftEffect <<< log)
    # flip runState 5
    # runBaseEffect
    # void