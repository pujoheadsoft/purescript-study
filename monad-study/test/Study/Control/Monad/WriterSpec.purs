module Test.Study.Control.Monad.WriterSpec where

import Prelude

import Data.Tuple (Tuple(..))
import Study.Control.Monad.Writer (Writer(..), runWriter, tell)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Writer" do
    describe "runWriter" do
      it "Writerの内容を取り出すことができる" do
        runWriter (Writer (Tuple 100 "value")) `shouldEqual` (Tuple 100 "value") 

    describe "Functorである" do
      it "内容に関数を適用することができる" do
        runWriter ((_ * 3) <$> Writer (Tuple 10 "X")) `shouldEqual` (Tuple 30 "X")

    describe "Applyである" do
      it "ログを蓄積しつつ、複数のWriterを引数にWriterの関数を適用することができる" do
        runWriter (Writer (Tuple (+) "a") <*> Writer (Tuple 10 "b") <*> Writer (Tuple 20 "c")) `shouldEqual`  (Tuple 30 "abc")
    
    describe "Applicativeである" do
      it "Writer型として返すことができる" do
        runWriter (pure 10) `shouldEqual` (Tuple 10 "")

    describe "tell" do
      it "結果の値と、Writerに蓄積した値をTupleで取得することができる" do
        (Tuple unit "hello") `shouldEqual` runWriter (tell "hello")

