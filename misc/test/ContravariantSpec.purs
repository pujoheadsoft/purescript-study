module Test.ContravariantSpec where

import Prelude

import Data.Functor.Contravariant (cmap)
import Data.Op (Op(..))
import Data.Predicate (Predicate(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

{-
class Contravariant f where
  cmap :: forall a b. (b -> a) -> f a -> f b

Functorと似ているが、関数が逆転している。
(a -> b) -> f a -> f b

-}

runOp :: forall a b. Op a b -> b -> a
runOp (Op f) = f

runPredicate :: forall a. Predicate a -> a -> Boolean
runPredicate (Predicate f) = f

spec :: Spec Unit
spec = do
  describe "Contravariant Test" do
    it "Op" do
      let
        -- Opは文字列化する関数を持っているので、cmapは文字列化できる何かを返す関数を受け取れる
        op = cmap (_ * 10) (Op show)
      runOp op 100 `shouldEqual` "1000"

    it "Predicate" do
      let
        -- Predicateは数値を比較している関数を持っているので、cmapは数値を返す関数を受け取れる。
        predicate = cmap (_ - 10) (Predicate (_ == 0))
      runPredicate predicate 10 `shouldEqual` true