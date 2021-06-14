module Main where

import Data.Maybe
import Data.Show (show)
import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log ("🍝" <> (show getValue))
  log ("🍝" <> (show getValue2))
  log ("🍝" <> (show getValue3))


getValue :: Maybe String
getValue = do
  x <- Just "x"
  y <- Just "y"
  Just (x <> y)

getValue2 :: Maybe String
getValue2 = (Just "x") >>= (\x -> (Just "y") >>= (\y -> (Just (x <> y))))

getValue3 :: Maybe String
getValue3 = (<>) <$> (Just "x") <*> (Just "y")
