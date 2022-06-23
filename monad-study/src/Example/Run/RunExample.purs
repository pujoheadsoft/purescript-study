module Example.Run.RunExample where

import Prelude

import Study.Control.Monad.Run.Run (EFFECT, Run)
import Study.Control.Monad.Run.State (STATE, gets)
import Type.Row (type (+))

type MyEffects =
  ( STATE Int
  + EFFECT
  + ()
  )

-- yesProgram :: Run MyEffects Unit
-- yesProgram = do
--   whenM (gets (_ > 0)) do
--     throw "Number is less than 0"
  