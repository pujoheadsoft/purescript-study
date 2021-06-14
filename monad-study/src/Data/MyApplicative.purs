module Data.MyApplicative where

import Data.MyApply (class MyApply)

class MyApply f <= MyApplicative f where
    pure :: forall a. a -> f a