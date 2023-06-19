module Test.ProfunctorSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Strong (fanout, splitStrong)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

{-
dimap :: forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d
->はProfunctorのインスタンスなので p を -> に置き換えた場合
(a -> b) -> (c -> d) -> (b -> c) -> (a -> d)
となる。
つまり関数3つを合成したようなものであり、3つめの関数は1つ目の関数と2つ目の関数との橋渡し役になっている。という構造。
-}

spec :: Spec Unit
spec = do
  describe "Produnctor Test" do
    it "dimap" do
      let
        fn = dimap (show :: Int -> String) Just ("add " <> _)
      fn 100 `shouldEqual` Just "add 100"

    describe "Strong" do
      it "splitStrong" do
        let
          -- a *** b でもよい
          -- Tupleの値それぞれに別の関数を適用する関数を作れる
          fn = splitStrong (show :: Int -> String) Just
        fn (Tuple 100 "value") `shouldEqual` (Tuple "100" (Just "value"))
      
      it "fanout" do
        let
          -- a &&& b でもよい
          -- 同じ値に別の関数を適用したTupleになる関数を作れる
          fn = fanout (show :: Int -> String) Just
        fn 100 `shouldEqual` (Tuple "100" (Just 100))