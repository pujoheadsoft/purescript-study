module Lens.Index where

import Prelude

import Data.Maybe (Maybe)
import Lens.Lens (lens)
import Lens.Prism.Maybe (_Just)
import Lens.Types (AffineTraversal')



class Index m a b | m -> a, m -> b where
  ix :: a -> AffineTraversal' m b

instance indexFn :: Eq i => Index (i -> a) i a where
  ix :: i -> AffineTraversal' (i -> a) a
  ix i = lens (\f -> f i) (\f a j -> if i == j then a else f j)

instance indexMaybe :: Index (Maybe a) Unit a where
  ix _ = _Just

-- instance indexArray :: Index (Array a) Int a where
--   ix n = affine