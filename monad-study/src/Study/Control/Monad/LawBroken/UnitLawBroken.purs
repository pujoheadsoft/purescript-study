module Study.Control.Monad.LawBroken.UnitLawBroken where

import Prelude

import Data.Maybe (Maybe(..))

newtype UnitLawBroken a = UnitLawBroken (Maybe a)

instance Show a => Show (UnitLawBroken a) where
  show (UnitLawBroken a) = show a

instance Eq a => Eq (UnitLawBroken a) where
  eq (UnitLawBroken a) (UnitLawBroken b) = a == b

instance Functor UnitLawBroken where
  map :: forall a b. (a -> b) -> UnitLawBroken a -> UnitLawBroken b
  map f (UnitLawBroken a) = UnitLawBroken (f <$> a)

instance Apply UnitLawBroken where
  apply :: forall a b. UnitLawBroken (a -> b) -> UnitLawBroken a -> UnitLawBroken b
  apply (UnitLawBroken f) (UnitLawBroken a) = UnitLawBroken (f <*> a)

instance Applicative UnitLawBroken where
  pure _ = UnitLawBroken Nothing

instance Bind UnitLawBroken where
  bind (UnitLawBroken a) f = case a of
    Just a' -> f a'
    Nothing -> UnitLawBroken Nothing

instance Monad UnitLawBroken
