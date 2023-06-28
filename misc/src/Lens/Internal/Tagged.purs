module Lens.Internal.Tagged where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)

newtype Tagged :: forall k. k -> Type -> Type
newtype Tagged a b = Tagged b

derive instance newtypeTagged :: Newtype (Tagged a b) _


instance taggedProfunctor :: Profunctor Tagged where
  dimap _ g (Tagged x) = Tagged (g x)


instance taggedChoice :: Choice Tagged where
  left  (Tagged x) = Tagged (Left x)
  right (Tagged x) = Tagged (Right x)