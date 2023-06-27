module Lens.Internal.Tagged where

import Prelude

import Data.Newtype (class Newtype)

newtype Tagged :: forall k. k -> Type -> Type
newtype Tagged a b = Tagged b

derive instance newtypeTagged :: Newtype (Tagged a b) _