module Study.Control.Monad.State where

import Prelude

import Data.Tuple (Tuple)

newtype State s a = State (s -> (Tuple a s))