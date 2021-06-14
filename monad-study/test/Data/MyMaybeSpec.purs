module Data.MyaMaybeSpec where

import Data.MyMaybe
import Prelude
import Test.Spec

import Data.MyApplicative (pure)
import Data.MyApply (apply, (<*>))
import Data.MyBind (bind, (>>=))
import Data.MyFunctor (fmap, (<$>))
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

nothing :: MyMaybe String
nothing = Nothing

nothingFn :: MyMaybe (String -> String)
nothingFn = Nothing

spec :: Spec Unit
spec = do
  describe "MyMaybeのテスト" do
    describe "Showの関数が実装されている" do
      it "Justの場合、その内容が取得できる" do
        show (Just "test") `shouldEqual` "(Just: \"test\")"

      it "Nothingの場合、Nothingという文字列が取得できる" do
        show nothing `shouldEqual` "Nothing"

    describe "Eqである" do
      it "Just同士の場合で、内容が同じならば同じとみなす" do
        (Just 9) `shouldEqual` (Just 9)

      it "Just同士の場合で、内容が異なる場合は異なるものとみなす" do
        (Just 1) `shouldNotEqual` (Just 2)

      it "Nothing同士ならば、同じとみなす" do
        nothing `shouldEqual` nothing

      it "JustとNothingの場合は、異なるものとみなす" do
        (Just "x") `shouldNotEqual` nothing

    describe "Functorである" do
      it "Justの場合、その内容に関数を適用することができる" do
        fmap (_ * 3) (Just 3) `shouldEqual` (Just 9)
        
      it "Justの場合、その内容に関数を適用することができる(左結合)" do
        ((_ * 3) <$> (Just 3)) `shouldEqual` (Just 9)

      it "Nothingの場合、関数を適用させても結果はNothingとなる" do
        fmap (_ <> "append") nothing `shouldEqual` nothing

    describe "Applyである" do
      it "Justの場合、その内容にJustの関数を適用することができる" do
        apply (Just (_ * 4)) (Just 2) `shouldEqual` (Just 8)

      it "nothingの場合、その内容にJustの関数を適用させても結果はNothingとなる" do
        apply (Just (_ <> "added")) nothing `shouldEqual` nothing

      it "Justにnothingの関数を適用させても結果はNothingとなる" do
        apply nothingFn (Just "x") `shouldEqual` nothing

      it "複数のJustを引数にJustの関数を適用することができる" do
        ((Just (+)) <*> Just 2 <*> Just 5) `shouldEqual` Just 7

      it "複数のJustの一つがNothingの場合Justの関数を適用しても結果はNothingとなる" do
        ((Just (<>)) <*> Just "x" <*> nothing) `shouldEqual` nothing

      it "複数のJustの内容を引数にJustの関数を適用することができる(Functorを利用)" do
        -- (+) <$> Just 2 の型は MyMaybe (Int -> Int)、つまりMyMaybeの関数、なので<*>でJust 5に適用可能。
        -- (関数はカリー化により(Just (_ + 2))という形になっている)
        ((+) <$> Just 2 <*> Just 5) `shouldEqual` Just 7

      it "複数のJustの内容を引数にJustの関数を適用することができる(引数3つ)" do
        ((\a b c -> (a + b + c)) <$> Just 1 <*> Just 2 <*> Just 3) `shouldEqual` Just 6

    describe "Applicativeである" do
      it "MyMaybe型として返すことができる" do
        pure "x" `shouldEqual` (Just "x")

    describe "Bindである" do
      it "第2引数の関数を第1引数の結果に適用させることができる" do
        bind (Just 3) (\n -> Just (n * 4)) `shouldEqual` Just 12

      it "第2引数の関数を第1引数の結果に適用させることを連鎖させられる" do
        ((Just 3) >>= (\n -> Just (n * 2)) >>= (\n -> Just (n * 2))) `shouldEqual` Just 12

      it "関数の適用に失敗した場合Nothingが返る" do
        ((Just "x") >>= (\_ -> Nothing) >>= (\n -> Just (n <> "a"))) `shouldEqual` nothing

    describe "Monadである" do
      it "左単位元則を満たしている" do
        -- 「ある値をpure関数に渡して、それをモナドの関数に与えた」場合、「そのモナドの関数を単にその値に適用した」場合と同じになる。
        -- この規則は、もしそれがdoブロックの最初の式であれば、pureの呼び出しを除去できると述べている
        (pure 10 >>= Just) `shouldEqual` (Just 10)

      it "右単位元則を満たしている" do
        -- 「モナド関数の適用結果をpureに関数に与えた」場合、「そのモナドの関数を単にその値に適用した」場合と同じになる。
        -- この規則は、もしそれがdoブロックの最後の式であれば、pureの呼び出しを除去できると述べている
        ((Just 10) >>= pure) `shouldEqual` (Just 10)

      it "結合則を満たしている" do
        -- 数学の結合法則と同じ。
        let 
          f = (\x -> Just (x + 3))
          g = (\y -> Just (y * 4))
          mx = Just 10
        ((mx >>= f) >>= g) `shouldEqual` (mx >>= (\x -> f x >>= g))