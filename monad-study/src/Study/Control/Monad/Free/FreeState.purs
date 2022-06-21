module Study.Control.Monad.Free.FreeState where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Study.Control.Monad.Free.Free (Free(..), liftF, resume)

data State s a = State (s -> s) (s -> a)

derive instance functorState :: Functor (State s)

type FreeState s a = Free (State s) a

state :: forall s a. (s -> a) -> FreeState s a
state f = liftF (State identity f)

runState :: forall s a. FreeState s a -> (s -> (Tuple a s))
runState f = \s -> (loop s f)
  where
  loop s f = case resume f of
    Left (State t k) ->
      let
        s' = t s
      in
        loop s' (k s')
    Right a ->
      (Tuple a s)

evalState :: forall s a. FreeState s a -> s -> a
evalState s i = fst (runState s i)

execState :: forall s a. FreeState s a -> s -> s
execState s i = snd (runState s i)