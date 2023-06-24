module Test.Lens.Internal.ForgetSpec where

import Prelude

import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.String (length)
import Lens.Internal.Forget (Forget(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Forget Test" do
    it "dimap" do
      let
        forget = Forget ((\i -> show i) :: Int -> String)

        -- diamp の identityの部分は Forget では使われないので型が合えば何でもいい
        f = unwrap $ dimap length identity forget

      f "value" `shouldEqual` "5"

