module Test.Study.Control.Monad.WriterSpec where

import Prelude
import Data.Tuple (Tuple(..))
import Study.Control.Monad.Writer (runWriter, tell)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Writer" do
    describe "tell" do
      it "結果の値と、Writerに蓄積した値をTupleで取得することができる" do
        (Tuple unit "hello") `shouldEqual` runWriter (tell "hello")