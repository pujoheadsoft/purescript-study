module Lens.Record where

import Prelude

import Data.Symbol (class IsSymbol)
import Lens.Lens (lens)
import Lens.Types (Lens)
import Prim.Row as Row
import Record (get, set)
import Type.Proxy (Proxy)


prop
  :: forall l r1 r2 r a b
   . IsSymbol l
  => Row.Cons l a r r1
  => Row.Cons l b r r2
  => Proxy l
  -> Lens (Record r1) (Record r2) a b
prop l = lens (get l) (flip (set l))