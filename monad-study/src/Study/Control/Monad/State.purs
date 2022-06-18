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

-- monadにさえなっていれば、monadのapを使って apply = ap と書くだけで済むが自前で書く
-- (ApplicativeかつBindであればMonadになれる)
instance applyState :: Apply (State s) where
  apply :: forall s a b. State s (a -> b) -> State s a -> State s b
  apply s1 s2 = State $ \s -> let
    (Tuple fn s') = runState s1 s
    (Tuple a ss) = runState s2 s'
    in (Tuple (fn a) ss)
  
instance applicativeState :: Applicative (State s) where
  pure :: forall s a. a -> State s a
  pure a = State $ \s -> (Tuple a s)

instance bindState :: Bind (State s) where
  bind :: forall s a b. State s a -> (a -> State s b) -> State s b
  bind m f = State $ \s -> let
    (Tuple a s') = runState m s
    in runState (f a) s'

instance monadStateInstance :: Monad (State s)

class Monad m <= MonadState s m | m -> s where
  get :: m s
  put :: s -> m Unit

instance monadState :: MonadState s (State s) where
  get :: (State s s)
  get = State $ \s -> (Tuple s s)

  put :: forall s. s -> (State s Unit)
  put s = State $ \_ -> (Tuple unit s)