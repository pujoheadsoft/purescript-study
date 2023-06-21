module Test.Lens.LensSpec where

import Prelude

import Lens.Lens (lens)
import Lens.Types (Lens')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type BoxRec = String
data Box = Box BoxRec

instance showBox :: Show Box where
  show (Box a) = a
instance eqBox :: Eq Box where
  eq (Box a) (Box b) = a == b

--_Box :: forall s a p. Strong p => p s a -> p (Box s) (Box a)
-- _Box = lens (\(Box a) -> a) (\_ -> Box)
_Box :: Lens' Box BoxRec
_Box = lens (\(Box a) -> a) (\(Box _) -> Box)

spec :: Spec Unit
spec = do
  describe "Lens Test" do
    it "lens" do
      let
        box = _Box (_ <> "Y") (Box "X")
      box `shouldEqual` Box "XY"