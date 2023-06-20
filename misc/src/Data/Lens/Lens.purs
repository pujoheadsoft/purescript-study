module Data.Lens.Lens where

import Prelude

import Data.Lens.Types (Lens)
import Data.Profunctor (dimap)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..))


lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = lens' \s -> Tuple (get s) \b -> set s b


lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)