module Test.Study.Control.Monad.LawBroken.LawBrokenSpec where

import Prelude
import Study.Control.Monad.LawBroken.LawBroken

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions ( shouldNotEqual)

spec :: Spec Unit
spec = do
  describe "Monad則" do
    it "壊れてること" do
      let 
        -- (m >>= id) >>= id
        left = (broken 1 *> broken 2) *> broken 3
        -- m >>= (\x -> id x >>= id)
        right = broken 1 *> (broken 2 *> broken 3)
      value left `shouldNotEqual` value right
