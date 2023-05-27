module Example.Free.SimpleFreeExample where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Study.Control.Monad.Free.SimpleFree (Free(..), foldFree, liftF)

data Teletype a =
    PutStrLn String a 
  | GetLine (String -> a)

derive instance teletypeFunctor :: Functor Teletype

type FreeTeletype = Free Teletype

putStrLn :: String -> FreeTeletype Unit
putStrLn s = liftF $ PutStrLn s unit

getLine :: FreeTeletype String
getLine = liftF $ GetLine identity

-- foldFreeを使わないパターン
run2 :: FreeTeletype ~> Effect
run2 (Pure a) = pure a
run2 (Free (PutStrLn s a)) = log s >>= \_ -> run a
run2 (Free (GetLine k)) = run $ k "SimpleFreeExample: fake input"

run :: FreeTeletype ~> Effect
run = foldFree go
  where
  go :: Teletype ~> Effect
  go (PutStrLn s a) = const a <$> log s
  go (GetLine k) = pure (k "SimpleFreeExample: fake input")

echo :: FreeTeletype Unit
echo = do
  a <- getLine
  putStrLn a
  putStrLn "SimpleFreeExample: Finished"

main :: Effect Unit
main = do
  run $ echo

