module Test.Pattern.ReaderT.ReaderTSpec where

import Prelude

import Control.Monad.State as State
import Control.Monad.Reader (class MonadReader, ReaderT, ask, runReaderT)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, new, read)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data Env = Env { 
  envLog :: String -> Effect Unit,
  envBalance :: Ref Int
}

class HasLog a where
  getLog :: a -> (String -> Effect Unit)
instance HasLog (String -> Effect Unit) where
  getLog = identity
else instance HasLog Env where
  getLog (Env {envLog}) = envLog

class HasBalance a where
  getBalance :: a -> Ref Int
instance HasBalance (Ref Int) where
  getBalance = identity
instance HasBalance Env where
  getBalance (Env {envBalance}) = envBalance

class Monad m <= MonadBalance m where
  modifyBalance :: (Int -> Int) -> m Unit
instance (HasBalance env, MonadEffect m) => MonadBalance (ReaderT env m) where
  modifyBalance f = do
    env <- ask
    liftEffect $ Ref.modify_ f (getBalance env)
instance Monad m => MonadBalance (State.StateT Int m) where
  modifyBalance = State.modify_

modify :: forall m. MonadBalance m => (Int -> Int) -> m Unit
modify f = do
  modifyBalance f

logSomething
  :: forall m env
   . HasLog env
  => MonadReader env m 
  => MonadEffect m
  => String
  -> m Unit
logSomething msg = do
  env <- ask
  liftEffect $ getLog env msg

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