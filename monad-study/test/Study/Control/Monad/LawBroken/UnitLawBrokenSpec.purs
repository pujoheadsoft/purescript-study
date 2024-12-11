module Test.Study.Control.Monad.LawBroken.UnitLawBrokenSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Study.Control.Monad.LawBroken.UnitLawBroken (UnitLawBroken(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec = do
  describe "単位元律を満たさない" do
    it "左単位元律を満たさない: pure a >>= f ≠ f a" do
      let
        a = "a"
        f = \x -> UnitLawBroken (Just x)
      (pure a >>= f) `shouldNotEqual` (f a)
    
    it "右単位元律を満たさない: m >>= pure ≠ m" do
      let m = UnitLawBroken (Just "a")
      (m >>= pure) `shouldNotEqual` m

  describe "結合律を満たす" do
    it "(m >>= f) >>= g == m >>= (\x -> f x >>= g)" do
      let
        m = UnitLawBroken (Just "a")
        f = \x -> UnitLawBroken (Just $ x <> "b")
        g = \x -> UnitLawBroken (Just $ x <> "c")
      ((m >>= f) >>= g) `shouldEqual` (m >>= (\x -> f x >>= g))
