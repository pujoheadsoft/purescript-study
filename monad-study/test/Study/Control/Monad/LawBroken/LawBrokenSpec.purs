module Test.Study.Control.Monad.LawBroken.LawBrokenSpec where

import Prelude

import Study.Control.Monad.LawBroken.LawBroken (LawBroken(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotEqual)

spec :: Spec Unit
spec = do
  describe "Monad則" do
    describe "結合律を満たさない" do
      it "(m >>= f) >>= g ≠ m >>= (\x -> f x >>= g)" do
        let
          m = LawBroken {r: 1, a: unit}
          f = \_ -> LawBroken {r: 2, a: unit}
          g = \_ -> LawBroken {r: 3, a: unit}
        ((m >>= f) >>= g) `shouldNotEqual` (m >>= (\x -> f x >>= g))
