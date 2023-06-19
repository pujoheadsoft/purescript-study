module Test.BifunctorSpec where

import Data.Bifunctor (bimap)
import Prelude

import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Bifunctor Test" do
    it "bimap Tuple" do
      let
        value = Tuple "value" 100
        actual = bimap (_ <> " appended") (_ + 10) value
        expected = Tuple "value appended" 110
      actual `shouldEqual` expected