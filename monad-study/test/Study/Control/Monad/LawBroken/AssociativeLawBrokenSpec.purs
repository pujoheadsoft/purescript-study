module Test.Study.Control.Monad.LawBroken.AssociativeLawBrokenSpec where

import Prelude

import Study.Control.Monad.LawBroken.AssociativeLawBroken (AssociativeLawBroken(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldNotEqual)

spec :: Spec Unit
spec = do
  describe "Monad則" do
    describe "結合律を満たさない" do
      it "(m >>= f) >>= g ≠ m >>= (\x -> f x >>= g)" do
        let
          m = AssociativeLawBroken {value: 1, a: unit}
          f = \a -> AssociativeLawBroken {value: 2, a}
          g = \a -> AssociativeLawBroken {value: 3, a}
        ((m >>= f) >>= g) `shouldNotEqual` (m >>= (\x -> f x >>= g))
