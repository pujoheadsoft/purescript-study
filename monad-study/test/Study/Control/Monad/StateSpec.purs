module Test.Study.Control.Monad.StateSpec where

import Prelude

import Data.Tuple (Tuple(..))
import Study.Control.Monad.State (State(..), runState, evalState, execState)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "State" do
    let
      fn = \x -> (Tuple 100 x)
    describe "runState" do
      it "値を取り出すことができる" do
        -- 関数自体の比較はできないため、関数の戻り値でassertする
        runState (State fn) "Value" `shouldEqual` (Tuple 100 "Value")
    
    describe "evalState" do
      it "runStateで評価した結果の「値」だけを返す" do
        evalState (State fn) "value" `shouldEqual` 100
    
    describe "execState" do
      it "runStateで評価した結果の「状態」だけを返す" do
        execState (State fn) "value" `shouldEqual` "value"

    describe "Functor" do
      it "関数を適用することができる" do
        runState ((_ * 10) <$> (State fn)) "Value" `shouldEqual` (Tuple 1000 "Value")
    
    describe "Apply" do
      it "関数を持つStateを使って関数を適用することができる" do
        let multiPly = \x -> (Tuple (*) x)
        runState ((State multiPly) <*> (State fn) <*> (State fn)) "Value" `shouldEqual` (Tuple 10000 "Value")
    
    describe "Applicative" do
      it "値を持つStateを作ることができる" do
        runState (pure 10) "Value" `shouldEqual` (Tuple 10 "Value")

    describe "Bind" do
      it "第1引数の結果に第2引数の関数を適用することができる" do
        runState (bind (pure 10) (\a -> pure (a + 10))) "Value" `shouldEqual` (Tuple 20 "Value")


