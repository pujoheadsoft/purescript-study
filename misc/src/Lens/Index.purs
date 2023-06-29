module Lens.Index where

import Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe, maybe)
import Lens.AffineTraversal (affineTraversal)
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

instance indexArray :: Index (Array a) Int a where
  ix n = affineTraversal set pre
    where
    set :: Array a -> a -> Array a
    set s b = fromMaybe s $ A.updateAt n b s

    -- `A.index s n`がJustを返したら`Right`を適用、Nothinだったら`Left s`をデフォルト値として返す
    pre :: Array a -> Either (Array a) a
    pre s = maybe (Left s) Right $ A.index s n