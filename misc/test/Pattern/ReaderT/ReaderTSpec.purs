module Test.Pattern.ReaderT.ReaderTSpec where

import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.State as State
import Effect.Class (liftEffect)
import Effect.Ref (new, read)
import Effect.Ref as Ref
import Pattern.ReaderT.ReaderT (logSomething, modify)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "ReaderT Pattern test" do
    describe "modify" do
      it "works, Effect" do
        liftEffect do
          var <- new 1
          runReaderT (modify (_ + 2)) var
          res <- read var
          res `shouldEqual` 3

      it "works, pure" do
        let
          res = State.execState (modify (_ + 2)) 1
        res `shouldEqual` 3

    describe "logSomething" $ do
      it "works" $ do
        liftEffect do
          var <- new ""
          let
            logFunc msg = Ref.modify_ (_ <> msg) var
            msg1 = "Hello "
            msg2 = "World\n"

          runReaderT (do
            logSomething msg1 
            logSomething msg2
          ) logFunc

          res <- read var
          res `shouldEqual` (msg1 <> msg2)