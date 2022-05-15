module Study.Control.Monad.Run.ReaderSpec
  ( spec
  )
  where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Study.Control.Monad.Run (Run, extract)
import Study.Control.Monad.Run.Reader (runReader, ask, READER)
import Type.Row (type (+))
import Debug (traceM)

readWithPlus10 :: forall r. Run (READER Int + r) Int
readWithPlus10 = do
  traceM ("readWithPlus10 before ask")
  x <- ask
  traceM ("readWithPlus10 after ask")
  pure (x + 10) -- RunはApplicativeを実装しているのでpureはRun。

spec :: Spec Unit
spec = do
  describe "Run Readerのテスト" do
    describe "askのテスト" do
      it "渡した環境の値を読み込んで使うことができる" do
        let
          -- runReaderの中で、readWithPlus10に100が渡されて実行される
          val = extract (runReader 100 readWithPlus10)
        val `shouldEqual` 110