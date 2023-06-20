module Test.ProfunctorSpec where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (fanin, splitChoice)
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
    
    describe "Choice" do
      it "splitChoice (Left)" do
        let
          fn = splitChoice (show :: Int -> String) Just
          value = Left 100 :: Either Int String
        fn value `shouldEqual` (Left "100")
      it "splitChoice (Right)" do
        let
          fn = splitChoice (show :: Int -> String) Just
          value = Right "100" :: Either Int String
        fn value `shouldEqual` (Right (Just "100"))

      it "funin (Left)" do
        let
          fn = fanin (show :: Int -> String) (show :: Boolean -> String)
          value = Left 100 :: Either Int Boolean
        fn value `shouldEqual` "100"
      it "funin (Right)" do
        let
          fn = fanin (show :: Int -> String) (show :: Boolean -> String)
          value = Right true :: Either Int Boolean
        fn value `shouldEqual` "true"