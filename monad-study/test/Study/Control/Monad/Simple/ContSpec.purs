module Test.Study.Control.Monad.Simple.ContSpec where

import Prelude

import Data.String (length)
import Study.Control.Monad.Simple.Cont (Cont, runCont)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Error = { cause :: String }

spec :: Spec Unit
spec = do
  describe "Contのテスト" do
    describe "Applicativeである" do
      it "pure" do
        let
          cont = pure@(Cont Int) "abc"
          v = runCont cont length
        v `shouldEqual` 3
    
    describe "Bindである" do
      it "bind" do
        let
          cont = pure@(Cont Int) 300 >>= \k -> pure@(Cont Int) (show k)
          v = runCont cont length
        v `shouldEqual` 3
