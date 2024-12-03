module Test.Study.Control.Monad.LawBroken.LawBrokenSpec where

import Prelude
import Study.Control.Monad.LawBroken.LawBroken (broken, value)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions ( shouldNotEqual)

spec :: Spec Unit
spec = do
  describe "Monad則" do
    it "壊れてること" do
      let 
        -- (m >>= f) >>= g
        left = (broken 1 *> broken 2) *> broken 3
        -- m >>= (\x -> f x >>= g)
        right = broken 1 *> (broken 2 *> broken 3)
      value left `shouldNotEqual` value right
