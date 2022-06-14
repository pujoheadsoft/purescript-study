module Study.Control.Monad.Writer where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Newtype (class Newtype)

newtype Writer w a = Writer (Tuple a w)

derive instance newtypeWriter :: Newtype (Writer w a) _

instance functorWriter :: Functor (Writer w) where
  map :: forall w a b. (a -> b) -> Writer w a -> Writer w b
  map f (Writer (Tuple a w)) = Writer (Tuple (f a) w)

runWriter :: forall w a. Writer w a -> (Tuple a w)
runWriter (Writer w) = w