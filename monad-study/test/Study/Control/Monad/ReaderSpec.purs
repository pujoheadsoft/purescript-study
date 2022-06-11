module Test.Study.Control.Monad.ReaderSpec where

import Prelude

import Study.Control.Monad.Reader (Reader, ask, asks, local, runReader, withReader)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Config = { debug :: Boolean }

spec :: Spec Unit
spec = do
  describe "Readerのテスト" do
    describe "ask" do
      it "環境を取得できる" do
        "value" `shouldEqual` runReader ask "value"
    
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
        false `shouldEqual` runReader getFlag { debug: false }
    
    describe "withReader" do
      it "環境の型とは別の型の値を、渡した関数内のスコープで扱うことができる(環境はIntだがStringを扱う)" do
        "0" `shouldEqual` runReader (withReader show ask) 0

      it "環境の型とは別の型の値を、渡した関数内のスコープで扱うことができる(環境はStringだがIntを扱う)" do
        0 `shouldEqual` runReader (withReader (const 0) ask) "group"
