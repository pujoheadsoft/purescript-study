module Lens.AffineTraversal where

import Prelude

import Data.Either (Either, either)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (right)
import Data.Profunctor.Strong (second, (&&&))
import Data.Tuple (Tuple(..))
import Lens.Types (AffineTraversal)



affineTraversal
  :: forall s t a b
   . (s -> b -> t)
  -> (s -> Either t a)
  -> AffineTraversal s t a b
affineTraversal set pre = affineTraversal' (set &&& pre)

affineTraversal'
  :: forall s t a b
   . (s -> Tuple (b -> t) (Either t a))
  -> AffineTraversal s t a b
affineTraversal' to pab =
  dimap to (\(Tuple b f) -> either identity b f) (second (right pab))