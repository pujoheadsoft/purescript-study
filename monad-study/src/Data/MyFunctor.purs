module Data.MyFunctor where

class MyFunctor f where
  fmap :: forall a b. (a -> b) -> f a -> f b

infixl 4 fmap as <$>
