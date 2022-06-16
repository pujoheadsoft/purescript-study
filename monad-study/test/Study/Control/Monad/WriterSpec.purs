module Test.Study.Control.Monad.WriterSpec where

import Prelude

import Data.Tuple (Tuple(..))
import Study.Control.Monad.Writer (Writer(..), listen, listens, pass, runWriter, tell, censor)
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

    describe "Bindである" do
      it "第1引数の結果に第2引数の関数を適用させることができる" do
        runWriter (bind (Writer (Tuple 100 "value")) (\a -> pure (a * 4))) `shouldEqual` (Tuple 400 "value")

    describe "tell" do
      it "結果の値と、Writerに蓄積した値をTupleで取得することができる" do
        runWriter (tell "hello") `shouldEqual` (Tuple unit "hello")

      it "bindの中でtellを使うことができる" do
        let 
          actual = runWriter $ do 
            tell "Hello"
            tell "World"
            pure 2022
        actual `shouldEqual` (Tuple 2022 "HelloWorld")

    describe "listen" do
      it "引数で与えた式のログの書き込み結果を取得することができる" do
        runWriter (listen (tell "hello")) `shouldEqual` (Tuple (Tuple unit "hello") "hello")

    describe "listens" do
      it "ログの取得結果に関数を適用することができる" do
        runWriter (listens (_ <> "World") (tell "Hello")) `shouldEqual` (Tuple (Tuple unit "HelloWorld") "Hello")

    describe "pass" do
      it "値として「値と関数のTuple」を渡すことで、ログの内容にその関数を適用することができる" do
        let
          p = pass do 
            tell "Hello"
            pure $ Tuple 100 (_ <> "World")
        runWriter p `shouldEqual` (Tuple 100 "HelloWorld")
    
    describe "censor" do
      it "ログの内容に適用する関数を渡したWriterのログに適用することができる" do
        -- passでやってることをもっと楽にできる
        runWriter (censor (_ <> "World") (tell "Hello")) `shouldEqual` (Tuple unit "HelloWorld")
        
