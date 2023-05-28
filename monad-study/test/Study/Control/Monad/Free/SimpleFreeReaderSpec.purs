module Test.Study.Control.Monad.Free.SimpleFreeReaderSpec where

import Prelude

import Study.Control.Monad.Free.SimpleFreeReader (FreeReader, ask, asks, local, runReader)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Config = { debug :: Boolean }

spec :: Spec Unit
spec = do
  describe "Reader(SimpleFree版)のテスト" do
    describe "ask" do
      it "環境を取得できる" do
        "value" `shouldEqual` runReader ask "value"
    
    describe "asks" do
      it "環境を取得する際に渡した関数で環境の値を変更することができる" do
        "value added" `shouldEqual` runReader (asks (_ <> " added")) "value"

    describe "local" do     
      it "環境の変更を、渡した関数内のスコープに閉じることができる" do
        let 
          getFlag :: FreeReader Config Boolean
          getFlag = do
            _ <- local (\c -> c { debug = true }) do
              -- ここでaskするとdebugはtrueになってる。
              pure false -- 説明のためだけの処理なので適当な値を返す
            c <- ask
            pure c.debug
        runReader getFlag { debug: false } `shouldEqual` false
