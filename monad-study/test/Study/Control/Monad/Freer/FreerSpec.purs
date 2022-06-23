module Study.Control.Monad.FreerSpec where

import Prelude
import Test.Spec (Spec, describe, it)

import Data.Exists (runExists)
import Study.Control.Monad.Freer.Freer (Freer(..), FreerBindF(..))
import Test.Spec.Assertions (shouldEqual)

data Value a = Value a

type FreerValueString = Freer Value String

showFreer :: FreerValueString -> String
showFreer = case _ of
  Bind e -> runExists (\(FreerBindF (Value v) x) -> showFreer (x v)) e
  Pure v -> v

spec :: Spec Unit
spec = do
  describe "Freerのテスト" do
    describe "Bindである" do
      it "取り込む型が何らかの値を保持できればBindになることができる" do
        let 
          value :: FreerValueString
          value = do
            a <- pure "value"
            b <- pure (a <> "Added")
            pure b
        showFreer value `shouldEqual` "valueAdded"