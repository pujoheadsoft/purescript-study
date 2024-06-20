module Test.Study.Control.Monad.Simple.ExceptSpec where

import Prelude

import Data.Either (Either(..))
import Study.Control.Monad.Simple.Except (Except(..), runExcept)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Error = { cause :: String }

spec :: Spec Unit
spec = do
  describe "Exceptのテスト" do
    describe "Applicativeである" do
      it "pure" do
        (Right "value") `shouldEqual` (runExcept $ pure@(Except Error) "value")
    
    describe "Bindである" do
      it "bind" do
        let
          ex = pure@(Except Error) "value" 
                >>= \v -> pure (v <> "X")
        (Right "valueX") `shouldEqual` (runExcept ex)
    
      it "bind(途中でエラーが発生した場合)" do
        let
          ex = pure@(Except Error) "value" 
                >>= \_ -> Except (Left ({cause: "error"}))
                >>= \v -> pure (v <> "X")
        (Left {cause: "error"}) `shouldEqual` (runExcept ex)