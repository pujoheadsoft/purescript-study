module Test.Study.Control.Monad.Free.FreeReaderSpec where

import Prelude

import Study.Control.Monad.Free.FreeReader (ask, asks, runReader)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Config = { debug :: Boolean }

spec :: Spec Unit
spec = do
  describe "Reader(Free版)のテスト" do
    describe "ask" do
      it "環境を取得できる" do
        "value" `shouldEqual` runReader ask "value"
    
    describe "asks" do
      it "環境を取得する際に渡した関数で環境の値を変更することができる" do
        "value added" `shouldEqual` runReader (asks (_ <> " added")) "value"

