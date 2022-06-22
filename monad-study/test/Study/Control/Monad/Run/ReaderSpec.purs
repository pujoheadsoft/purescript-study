module Study.Control.Monad.Run.ReaderSpec
  ( spec
  )
  where

import Prelude

import Debug (traceM)
import Study.Control.Monad.Run.Run (Run, extract)
import Study.Control.Monad.Run.Reader (runReader, ask, READER)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Row (type (+))

{-
  こう書いても同じ。最初はBind型でないとあかん。askが返すRunはBindでもあるのでOK。
  ask >>= (\x -> pure (x + 10))
  bind ask (\x -> pure (x + 10))
-}
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
          -- runReaderの中で、readWithPlus10に100が渡されて実行される
        extract (runReader readWithPlus10 100) `shouldEqual` 110
