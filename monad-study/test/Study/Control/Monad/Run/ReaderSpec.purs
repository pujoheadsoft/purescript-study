module Study.Control.Monad.Run.ReaderSpec where

import Prelude
import Test.Spec
import Test.Spec.Assertions (shouldEqual)
import Study.Control.Monad.Run (Run(..), extract)
import Study.Control.Monad.Run.Reader (runReader, ask, READER)
import Type.Row (type (+))

readWithPlus10 :: forall r. Run (READER Int + r) Int
readWithPlus10 = do
  x <- ask
  pure (x + 10)

spec :: Spec Unit
spec = do
  describe "Run Readerのテスト" do
    describe "askのテスト" do
      it "渡した環境の値を読み込んで使うことができる" do
        let
          val = extract (runReader 100 readWithPlus10)
        val `shouldEqual` 110