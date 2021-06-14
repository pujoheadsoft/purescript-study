module Data.MyBind where

import Data.MyApply

class MyApply m <= MyBind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

infixl 1 bind as >>=