module Test.Lens.Internal.WanderSpec where

import Prelude

import Data.Traversable (traverse)
import Lens.Internal.Wander (wander)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Wander Test" do
    it "wander" do
      let
        -- wander の `t` は
        -- forall f.     Applicative f => (a -> f b) -> s -> f t
        -- という定義なので
        -- traverseの
        -- forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
        -- に合う
        f = wander traverse
      f ((+) 1.0) [1.0,2.0,3.0] `shouldEqual` [2.0, 3.0, 4.0]