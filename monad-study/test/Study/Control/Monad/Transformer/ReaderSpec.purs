module Test.Study.Control.Monad.Transformer.ReaderSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Study.Control.Monad.Transformer.Reader (Reader, ReaderT, ask, asks, local, runReader, runReaderT, withReader)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Config = { debug :: Boolean }

spec :: Spec Unit
spec = do
  describe "Readerのテスト" do
    describe "ask" do
      it "環境を取得できる" do
        runReader ask "value" `shouldEqual` "value"
    
    describe "asks" do
      it "環境を取得する際に渡した関数で環境の値を変更することができる" do
        "value added" `shouldEqual` runReader (asks (_ <> " added")) "value"

    describe "local" do     
      it "環境の変更を、渡した関数内のスコープに閉じることができる" do
        let 
          getFlag :: Reader Config Boolean
          getFlag = do
            _ <- local (\c -> c { debug = true }) do
              -- ここでaskするとdebugはtrueになってる。
              pure false -- 説明のためだけの処理なので適当な値を返す
            c <- ask
            pure c.debug
        runReader getFlag { debug: false } `shouldEqual` false
    
    describe "withReader" do
      it "環境の型とは別の型の値を、渡した関数内のスコープで扱うことができる(環境はIntだがStringを扱う)" do
        runReader (withReader show ask) 0 `shouldEqual` "0"

      it "環境の型とは別の型の値を、渡した関数内のスコープで扱うことができる(環境はStringだがIntを扱う)" do
        runReader (withReader (const 0) ask) "group" `shouldEqual` 0

    describe "ReaderT" do
      it "モナドを合成できる" do
        let
          m :: ReaderT Int Maybe Int
          m = do
            value <- ask
            pure (value + 10)
        (runReaderT m 10) `shouldEqual` (Just 20)