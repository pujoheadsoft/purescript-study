module Test.Study.Control.Monad.StateSpec where

import Prelude

import Data.Tuple (Tuple(..))
import Study.Control.Monad.State (State(..), runState)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "State" do
    describe "runState" do
      it "値を取り出すことができる" do
        -- 関数自体の比較はできないため、関数の戻り値でassertする
        let
          fn = State $ \x -> (Tuple unit x)
        (runState fn) "Value" `shouldEqual` (Tuple unit "Value")