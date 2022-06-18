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

