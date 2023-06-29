module Test.Lens.IndexSpec where

import Prelude

import Lens.Index (ix)
import Lens.Lens (lens)
import Lens.Types (Lens')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec = do
  describe "Index Test" do
    it "ix" do
      let
        box = (ix 1) 
      "" `shouldEqual` ""