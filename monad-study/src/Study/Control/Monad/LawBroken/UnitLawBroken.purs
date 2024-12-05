module Study.Control.Monad.LawBroken.UnitLawBroken where

import Prelude

import Data.Maybe (Maybe(..))

newtype UnitLawBroken a = UnitLawBroken (Maybe a)

instance showUnitLawBroken :: Show a => Show (UnitLawBroken a) where
  show (UnitLawBroken a) = show a

instance eqUnitLawBroken :: Eq a => Eq (UnitLawBroken a) where
  eq (UnitLawBroken a) (UnitLawBroken b) = a == b

instance functorUnitLawBroken :: Functor UnitLawBroken where
  map :: forall a b. (a -> b) -> UnitLawBroken a -> UnitLawBroken b
  map f (UnitLawBroken a) = UnitLawBroken (f <$> a)

instance applyUnitLawBroken :: Apply UnitLawBroken where
  apply :: forall a b. UnitLawBroken (a -> b) -> UnitLawBroken a -> UnitLawBroken b
  apply (UnitLawBroken f) (UnitLawBroken a) = UnitLawBroken (f <*> a)

instance applicativeUnitLawBroken :: Applicative UnitLawBroken where
  pure :: forall a. a -> UnitLawBroken a
  pure _ = UnitLawBroken Nothing

instance bindUnitLawBroken :: Bind UnitLawBroken where
  bind :: forall a b. UnitLawBroken a -> (a -> UnitLawBroken b) -> UnitLawBroken b
  bind (UnitLawBroken a) f = case a of
    Just a' -> f a'
    Nothing -> UnitLawBroken Nothing

instance monadUnitLawBroken :: Monad UnitLawBroken

