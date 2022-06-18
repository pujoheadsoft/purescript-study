module Study.Control.Monad.State where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple (Tuple, fst, snd)

--
-- s 実行前の状態
-- -> 
-- (Tuple
--   a 実行結果
--   s 実行後の状態
-- )
newtype State s a = State (s -> (Tuple a s))

derive instance newtypeState :: Newtype (State s a) _

runState :: forall s a. State s a -> (s -> (Tuple a s))
runState (State f) = f

evalState :: forall s a. State s a -> s -> a
evalState s i = fst (runState s i)

execState :: forall s a. State s a -> s -> s
execState s i = snd (runState s i)