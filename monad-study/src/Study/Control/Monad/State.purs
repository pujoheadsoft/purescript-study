module Study.Control.Monad.State where

import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)

newtype State s a = State (s -> (Tuple a s))

derive instance newtypeState :: Newtype (State s a) _

runState :: forall s a. State s a -> (s -> (Tuple a s))
runState (State f) = f