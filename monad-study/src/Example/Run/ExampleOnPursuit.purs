module Example.Run.ExampleOnPursuit where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Foldable (oneOfMap)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Study.Control.Monad.Run.Choose (CHOOSE, runChoose)
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
  whenM (gets (_ < 0)) do
    throw "Number is less than 0"
  whileM_ (gets (_ > 0)) do
    x <- gets show
    liftEffect $ log $ "Yes: " <> x
    modify (_ - 1)
  where
  whileM_
    :: forall a
     . Run MyEffects Boolean
    -> Run MyEffects a
    -> Run MyEffects Unit
  whileM_ mb ma = flip tailRecM unit \_ ->
    -- mbがtrueだったらloop、falseだったら終了
    mb >>= if _ then ma $> Loop unit else pure $ Done unit

chooseProgram :: forall r. Run (CHOOSE + EFFECT + r) Int
chooseProgram = do
  n <- oneOfMap pure [ 1, 2, 3, 4, 5 ]
  liftEffect $ log $ show n
  pure (n + 1)

  
main :: Effect Unit
main = do
  log("yesProgram Start ---------------")
  yesProgram
    # catch (liftEffect <<< log)
    # flip runState 5
    # runBaseEffect
    # void
  log("yesProgram End -----------------")

  log("chooseProgram Start ---------------")
  as <- chooseProgram
    # runChoose
    # runBaseEffect
  logShow (as :: Array Int)
  log("chooseProgram End ---------------")
