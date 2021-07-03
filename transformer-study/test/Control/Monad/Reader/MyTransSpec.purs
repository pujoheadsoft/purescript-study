module Control.Monad.Reader.MyTransSpec where

import Prelude
import Test.Spec

import Control.Monad.Reader.MyClass (ask, local)
import Control.Monad.Reader.MyTrans (ReaderT(..), runReaderT)
import Data.Maybe (Maybe(..))
import Data.String (length)
import Test.Spec.Assertions (shouldEqual)

type TestReaderT = ReaderT String Maybe String

testReaderT :: TestReaderT
testReaderT = ReaderT (\x -> Just x) -- ただ包んで返すだけ

spec :: Spec Unit
spec = do
  describe "ReaderTのテスト" do
    {-
      テスト中で runReaderT testReaderT "test" としている箇所は、runReaderTでtestReaderTの計算を実行している。
      testReaderTは渡した内容をJustで包んで返すだけの実装なので、この場合結果はJust "test"となる
    -}
    describe "Functorである" do
      it "内容に関数を適用することができる" do
        let actual = (_ <> "!") <$> testReaderT -- !を結合する関数をかます
        runReaderT actual "test" `shouldEqual` Just "test!"
    
    describe "Applyである" do
      it "内容に関数を適用することができる" do
        let actual = ((\a b c -> a <> b <> c) <$> testReaderT <*> testReaderT <*> testReaderT) -- 3つの同じreaderの内容同士を結合する
        runReaderT actual "test" `shouldEqual` Just "testtesttest"

    describe "Applicativeである" do
      it "pureでReaderTを返すことができる" do
        pure "test" `shouldEqual` runReaderT testReaderT "test"

    describe "Bindである" do
      it "第2引数の関数を第1引数の結果に適用させることを連鎖させられる" do
        let actual = (testReaderT >>= (\_ -> ReaderT (\x -> Just (x <> "!")))) 
        runReaderT actual "test" `shouldEqual` Just "test!"

    describe "MonadAskである" do
      it "askで環境の値を問い合わせることができる" do
        let
          (fun :: TestReaderT) = do
            value <- ask
            pure $ value <> "!!"
        runReaderT fun "test" `shouldEqual` Just "test!!"

    describe "MonadReaderである" do
      it "localで環境の値を書き換えた状態で実行することができる" do
        let
          (fun :: TestReaderT) = local (\v -> v <> "add") testReaderT -- 環境の値を変えて計算を実行
        runReaderT fun "test" `shouldEqual` Just "testadd"