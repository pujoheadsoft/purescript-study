module Study.Control.Monad.Free.SimpleFreeSpec where

import Prelude
import Test.Spec (Spec, describe, it)

import Study.Control.Monad.Free.SimpleFree (Free(..))
import Test.Spec.Assertions (shouldEqual)

type FreeValueString = Free Value String

data Value a = Value a

-- Freeの型はFunctorだけ実装していればMonadになれることを検証するため、Functorだけ実装する
instance valueFunctor :: Functor Value where
  map f (Value v) = Value (f v)

showFree :: FreeValueString -> String
showFree = case _ of
  Free f -> case f of Value v -> showFree v
  Pure v -> v

spec :: Spec Unit
spec = do
  describe "SimpeFreeのテスト" do
    let f = Free (Value (Pure "Value"))
    describe "Functorである" do
      it "取り込む型がFunctorを実装していれば、FreeもFunctorになることができる" do
        showFree ((_ <> "Added") <$> f) `shouldEqual` "ValueAdded"
    
    describe "Applyである" do
      it "取り込む型がFunctorを実装していれば、FreeはApplyになることができる" do
        showFree ((\x y -> x <> y) <$> f <*> f) `shouldEqual` "ValueValue"

    describe "Monadである" do
      it "取り込む型がFunctorを実装していれば、FreeはMonadになることができる" do
        showFree (f >>= (\v -> Pure (v <> "Added"))) `shouldEqual` "ValueAdded"

