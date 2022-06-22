module Test.Study.Control.Monad.Run.WriterSpec where

import Prelude

import Data.Tuple (Tuple(..))
import Study.Control.Monad.Run.Run (extract)
import Study.Control.Monad.Run.Writer (writer, runWriter, tell, censor)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Writer" do
    describe "runWriter" do
      it "Writerの内容を取り出すことができる" do
        extract (runWriter (writer (Tuple 100 "value"))) `shouldEqual` (Tuple 100 "value") 

    describe "Functorである" do
      it "内容に関数を適用することができる" do
        extract (runWriter ((_ * 3) <$> writer (Tuple 10 "X"))) `shouldEqual` (Tuple 30 "X")

    describe "Applyである" do
      it "ログを蓄積しつつ、複数のWriterを引数にWriterの関数を適用することができる" do
        extract (runWriter (writer (Tuple (+) "a") <*> writer (Tuple 10 "b") <*> writer (Tuple 20 "c"))) `shouldEqual` (Tuple 30 "abc")

    describe "Applicativeである" do
      it "Writer型として返すことができる" do
        extract (runWriter (pure 10)) `shouldEqual` (Tuple 10 "")

    describe "Bindである" do
      it "第1引数の結果に第2引数の関数を適用させることができる" do
        extract (runWriter (bind (writer (Tuple 100 "value")) (\a -> pure (a * 4)))) `shouldEqual` (Tuple 400 "value")

    describe "tell" do
      it "結果の値と、Writerに蓄積した値をTupleで取得することができる" do
        extract (runWriter (tell "hello")) `shouldEqual` (Tuple unit "hello")

      it "bindの中でtellを使うことができる" do
        let 
          actual = runWriter $ do 
            tell "Hello"
            tell "World"
            pure 2022
        extract actual `shouldEqual` (Tuple 2022 "HelloWorld")
  
    describe "censor" do
      it "ログの内容に適用する関数を渡したWriterのログに適用することができる" do
        -- passでやってることをもっと楽にできる
        extract (runWriter (censor (_ <> "World") (tell "Hello"))) `shouldEqual` (Tuple unit "HelloWorld")
        

