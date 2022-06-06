module Main where

import Data.Maybe
import Prelude

import Data.Show (show)
import Debug (debugger, trace, traceM)
import Effect (Effect)
import Effect.Console (log)
import Effect.Console as Console
import Example.Free.Teletype as T
import Example.Free.Teletype2 as T2
import Study.Control.Monad.Run (Run, extract, runCont)
import Study.Control.Monad.Run as Run
import Study.Control.Monad.Run.Reader (runReader, ask, READER)
import Type.Row (type (+))

-- main :: Effect Unit
-- main = do
--   log ("🍝" <> (show getValue))
--   log ("🍝" <> (show getValue2))
--   log ("🍝" <> (show getValue3))


getValue :: Maybe String
getValue = do
  x <- Just "x"
  y <- Just "y"
  Just (x <> y)

getValue2 :: Maybe String
getValue2 = (Just "x") >>= (\x -> (Just "y") >>= (\y -> (Just (x <> y))))

getValue3 :: Maybe String
getValue3 = (<>) <$> (Just "x") <*> (Just "y")

-- これはRunを返している
readWithPlus :: forall r. Run (READER Int + r) Int
readWithPlus = do
--  traceM({m: "readWithPlus10: ask (before)"})
  x <- ask
  y <- pure (x + 10)
--  traceM({m: "readWithPlus10: ask (after)", x: x})
  pure (y + 20) -- RunはApplicativeを実装しているのでpureはRun。
    
main :: Effect Unit
main = do
  T.main
  T2.main
  trace(extract (runReader 100 readWithPlus)) \_ -> log("")
