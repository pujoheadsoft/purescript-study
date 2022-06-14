module Study.Control.Monad.Writer where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Newtype (class Newtype)

newtype Writer w a = Writer (Tuple a w)

derive instance newtypeWriter :: Newtype (Writer w a) _

instance functorWriter :: Functor (Writer w) where
  map :: forall w a b. (a -> b) -> Writer w a -> Writer w b
  map f (Writer (Tuple a w)) = Writer (Tuple (f a) w)

instance applyWriter :: Semigroup w => Apply (Writer w) where
  apply :: forall a b w. Semigroup w => Writer w (a -> b) -> Writer w a -> Writer w b
  apply (Writer (Tuple f w)) (Writer (Tuple a w')) = Writer (Tuple (f a) (w <> w'))

instance applicativeWriter :: Monoid w => Applicative (Writer w) where
  pure :: forall a w. Monoid w => a -> Writer w a
  pure a = Writer (Tuple a mempty)

tell :: forall a. a -> Writer a Unit
tell a = Writer (Tuple unit a)

runWriter :: forall w a. Writer w a -> (Tuple a w)
runWriter (Writer w) = w