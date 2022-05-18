module Main where

import Data.Maybe
import Data.Show (show)
import Prelude

import Effect (Effect)
import Effect.Console (log)

import Study.Control.Monad.Run (Run, extract)
import Study.Control.Monad.Run.Reader (runReader, ask, READER)
import Type.Row (type (+))
import Debug (traceM, trace, debugger)

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
readWithPlus10 :: forall r. Run (READER Int + r) Int
readWithPlus10 = do
  traceM({m: "readWithPlus10: ask (before)"})
  x <- ask
  traceM({m: "readWithPlus10: ask (after)", x: x})
  pure (x + 10) -- RunはApplicativeを実装しているのでpureはRun。

main :: Effect Unit
main = do
  -- traceM({m: "main", r: readWithPlus10})
  -- trace({m: "main", r: readWithPlus10}) \_ -> log("")
  trace(extract (runReader 100 readWithPlus10)) \_ -> log("")