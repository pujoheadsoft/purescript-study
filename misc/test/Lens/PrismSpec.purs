module Test.Lens.PrismSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Lens.Prism (prism', review)
import Lens.Types (Prism')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data XYZ
  = X Int
  | Y
  | Z

instance showXYZ :: Show XYZ where
  show = case _ of
    (X v) -> show v
    Y -> "Y"
    Z -> "Z"

instance eqXYZ :: Eq XYZ where
  eq (X a) (X b) = a == b
  eq Y Y = true
  eq Z Z = true
  eq _ _ = false

_X :: Prism' XYZ Int
_X = prism' X case _ of
  (X v) -> Just v
  _ -> Nothing

spec :: Spec Unit
spec = do
  describe "Prism Test" do
    it "review" do
      let
        actual = review _X 100
      actual `shouldEqual` X 100
    