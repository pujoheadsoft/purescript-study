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
      fn = State $ \x -> (Tuple unit x)
    describe "runState" do
      it "値を取り出すことができる" do
        -- 関数自体の比較はできないため、関数の戻り値でassertする
        (runState fn) "Value" `shouldEqual` (Tuple unit "Value")
    
    describe "evalState" do
      it "runStateで評価した結果の「値」だけを返す" do
        (evalState fn) "value" `shouldEqual` unit
    
    describe "execState" do
      it "runStateで評価した結果の「状態」だけを返す" do
        (execState fn) "value" `shouldEqual` "value"