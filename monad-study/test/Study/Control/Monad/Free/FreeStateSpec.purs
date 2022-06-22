module Test.Study.Control.Monad.Free.FreeStateSpec where

import Prelude

import Data.Tuple (Tuple(..))
import Study.Control.Monad.Free.FreeState (evalState, execState, get, gets, modify, put, runState, state)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "State(Free版)" do
    let
      fn = \_ -> 100
    describe "runState" do
      it "値を取り出すことができる" do
        -- 関数自体の比較はできないため、関数の戻り値でassertする
        runState (state fn) "Value" `shouldEqual` (Tuple 100 "Value")
    
    describe "evalState" do
      it "runStateで評価した結果の「値」だけを返す" do
        evalState (state fn) "value" `shouldEqual` 100
    
    describe "execState" do
      it "runStateで評価した結果の「状態」だけを返す" do
        execState (state fn) "value" `shouldEqual` "value"

    describe "Functor" do
      it "関数を適用することができる" do
        runState ((_ * 10) <$> (state fn)) "Value" `shouldEqual` (Tuple 1000 "Value")
    
    describe "Apply" do
      it "関数を持つStateを使って関数を適用することができる" do
        let multiPly = \_ -> (*)
        runState ((state multiPly) <*> (state fn) <*> (state fn)) "Value" `shouldEqual` (Tuple 10000 "Value")
    
    describe "Applicative" do
      it "値を持つStateを作ることができる" do
        runState (pure 10) "Value" `shouldEqual` (Tuple 10 "Value")

    describe "Bind" do
      it "第1引数の結果に第2引数の関数を適用することができる" do
        runState (bind (pure 10) (\a -> pure (a + 10))) "Value" `shouldEqual` (Tuple 20 "Value")

    describe "get" do
      it "状態を新しい値として取得できる" do
        -- Tupleの一つ目が取得できた値(用途として値がほしいだけならevalStateを使えばよい)
        runState (get) "Value" `shouldEqual` (Tuple "Value" "Value")

    describe "put" do
      it "状態を書き換えられる" do
        runState (put "Update") "Value" `shouldEqual` (Tuple unit "Update")

    describe "gets" do
      it "渡した関数を適用した値を取得できる" do
        runState (gets (_ + 1)) 1 `shouldEqual` (Tuple 2 1)
    
    describe "modify" do
      it "渡した関数を使って状態を変更することができる" do
        runState (modify (_ + 1)) 1 `shouldEqual` (Tuple unit 2)
