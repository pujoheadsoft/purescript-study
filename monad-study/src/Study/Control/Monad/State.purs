module Study.Control.Monad.State where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)

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

instance functorState :: Functor (State s) where
  map :: forall s a b. (a -> b) -> State s a -> State s b
  map f m = State $ \s -> let 
    (Tuple a s') = runState m s
    in (Tuple (f a) s')

instance applyState :: Apply (State s) where
  apply :: forall s a b. State s (a -> b) -> State s a -> State s b
  apply s1 s2 = State $ \s -> let
    (Tuple fn s') = runState s1 s
    (Tuple a ss) = runState s2 s'
    in (Tuple (fn a) ss)