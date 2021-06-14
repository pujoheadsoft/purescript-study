module Data.MyApply where

import Control.Category (identity)
import Data.Function (const)
import Data.MyFunctor (class MyFunctor, (<$>))

class MyFunctor f <= MyApply f where
  apply :: forall a b. f (a -> b) -> f a -> f b

infixl 4 apply as <*>


applySecond :: forall a b f. MyApply f => f a -> f b -> f b
applySecond a b = const identity <$> a <*> b

-- Haskell の >> と同じもの
infixl 4 applySecond as *>