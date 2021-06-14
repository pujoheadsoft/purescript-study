module Data.MyMaybe where

import Data.MyApplicative
import Data.MyApply
import Data.MyFunctor
import Data.MyBind
import Data.Semigroup
import Data.Show
import Data.Eq

-- import Data.Maybe (Maybe)

data MyMaybe a
  = Just a
  | Nothing

instance showMyMaybe :: Show a => Show (MyMaybe a) where
  show (Just x) = "(Just: " <> show x <> ")"
  show Nothing = "Nothing"

derive instance eqMyMaybe :: Eq a => Eq (MyMaybe a)
instance eq1MyMaybe :: Eq1 MyMaybe where eq1 = eq

instance functorMyMaybe :: MyFunctor MyMaybe where
  fmap fn (Just x) = Just (fn x)
  fmap _ _ = Nothing

instance applyMyMaybe :: MyApply MyMaybe where
  apply (Just fn) x = fn <$> x
  apply Nothing _ = Nothing

instance applicativeMyMaybe :: MyApplicative MyMaybe where
  pure = Just

instance bindMyMaybe :: MyBind MyMaybe where
  bind (Just a) fn = fn a
  bind Nothing _ = Nothing

