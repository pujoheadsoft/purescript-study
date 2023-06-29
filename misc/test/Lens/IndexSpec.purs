module Test.Lens.IndexSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Lens.Fold ((^?))
import Lens.Index (ix)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

{-
  `Fold`の`previewOn`のテストも兼ねている
-}
spec :: Spec Unit
spec = do
  describe "Index Test" do
    it "ix" do
      let
        actual = ([1, 2, 3] :: Array Int) ^? ix 2
      actual `shouldEqual` Just 3

    it "ix (indexの範囲外)" do
      let
        actual = ([1, 2, 3] :: Array Int) ^? ix 3
      actual `shouldEqual` Nothing