module Study.Control.Monad.EffSpec where

import Prelude
import Test.Spec

import Type.Row (type (+))
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)
import Data.Exists (runExists)
import Data.Maybe (Maybe(..))
import Study.Control.Monad.Eff (Eff(..), EffBindF(..))
import Test.Spec.Assertions (shouldEqual)

newtype Reader1 e a = Reader1 (e -> a)

type READER1 e r = (reader1 :: Reader1 e | r)

newtype Reader2 e a = Reader2 (e -> a)

type READER2 e r = (reader2 :: Reader2 e | r)

-- ask :: (Reader e) effs => Eff effs e
-- ask = send Reader

spec :: Spec Unit
spec = do
  describe "Freerのテスト" do
    describe "Bindである" do
      it "取り込む型が何らかの値を保持できればBindになることができる" do
        "" `shouldEqual` ""