module Lens.Prism where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (under)
import Data.Profunctor (dimap, rmap)
import Data.Profunctor.Choice (right)
import Lens.Internal.Tagged (Tagged(..))
import Lens.Types (Prism, Prism', Review)


prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism to fro pab = dimap fro (either identity identity) (right (rmap to pab))


prism' :: forall s a. (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' to fro = prism to (\s -> maybe (Left s) Right (fro s))

{-
  展開するとこうなる
  (Tagged a b -> Tagged s t) -> b -> t
  `under`の定義から、`(Tagged a b -> Tagged s t) -> b`を渡すと`t`が返される。
-}
review :: forall s t a b. Review s t a b -> b -> t
review = under Tagged
