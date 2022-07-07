module Data.RowSpec where

import Prelude
import Test.Spec

import Data.Row (get, set)
import Type.Proxy (Proxy(..))
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Rowのテスト" do
    describe "getのテスト" do
      it "Rowに存在するシンボルを指定して値を取得することができる" do
         get (Proxy :: Proxy "x") { x: 100 } `shouldEqual` 100

    describe "setのテスト" do
      it "Rowに対してシンボルを指定して値を設定することができる" do
        set (Proxy :: Proxy "x") 110 { x: 10} `shouldEqual` { x: 110 }
