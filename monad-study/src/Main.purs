module Main where

import Prelude

import Debug (trace)
import Effect (Effect)
import Effect.Console (log)
import Example.Free.Teletype as T
import Study.Control.Monad.Free.FreeReader as FR
import Study.Control.Monad.Simple.Reader as R
import Study.Control.Monad.Run (Run, extract)
import Study.Control.Monad.Run.Reader (runReader, ask, READER)
import Type.Row (type (+))

readWithPlus :: R.Reader String String
readWithPlus = do
  value <- R.ask
  pure (value <> " Added!")

readWithPlusFree :: FR.FreeReader String String
readWithPlusFree = do
  value <- FR.ask
  pure (value <> " Added2!")

-- これはRunを返している
readWithPlusRun :: forall r. Run (READER Int + r) Int
readWithPlusRun = do
--  traceM({m: "readWithPlus10: ask (before)"})
  x <- ask
  y <- pure (x + 10)
--  traceM({m: "readWithPlus10: ask (after)", x: x})
  pure (y + 20) -- RunはApplicativeを実装しているのでpureはRun。

main :: Effect Unit
main = do
  T.main
  log $ R.runReader readWithPlus "hoge1"
  log $ FR.runReader readWithPlusFree "hoge1"
  trace(extract (runReader 100 readWithPlusRun)) \_ -> log("")
