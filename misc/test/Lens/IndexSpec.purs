module Test.Lens.IndexSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Lens.Fold ((^?))
import Lens.Index (ix)
import Lens.Setter ((.~))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

{-
  `Fold`の`previewOn`のテストも兼ねている
-}
spec :: Spec Unit
spec = do
  describe "Index Test" do
    describe "Get" do
      it "ix" do
        let
          actual = [1, 2, 3] ^? ix 2
        actual `shouldEqual` Just 3

      it "ix (indexの範囲外)" do
        let
          actual = [1, 2, 3] ^? ix 3
        actual `shouldEqual` Nothing

    describe "Set" do
      it "ix" do
        let
          actual = [1, 2, 3] # ix 2 .~ 1000
        actual `shouldEqual` [1, 2, 1000]

      it "ix (indexの範囲外)" do
        let
          actual = [1, 2, 3] # ix 3 .~ 1000
        actual `shouldEqual` [1, 2, 3]