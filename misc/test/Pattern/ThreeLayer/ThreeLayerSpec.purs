module Test.Pattern.ThreeLayer.ThreeLayerSpec where

import Prelude

import Control.Monad.Reader (class MonadAsk, Reader, ask, runReader)
import Data.Either (Either(..))
import Pattern.ThreeLayer.Layer2 (class GetUserName, class LogToScreen, program)
import Pattern.ThreeLayer.Layer3 (Name(..))
import Test.PMock (any, fun, mock, verifySequence, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

{-
  Layer 1 のテスト用実装は必要
-}

newtype TestM a = TestM (Reader TestFunctions a)
derive newtype instance functorTestM     :: Functor TestM
derive newtype instance applyTestM       :: Apply TestM
derive newtype instance Applicative TestM
derive newtype instance bindTestM        :: Bind TestM
derive newtype instance monadTestM       :: Monad TestM
derive newtype instance monadAsk         :: MonadAsk TestFunctions TestM

runTest :: forall a. TestM a -> TestFunctions -> a
runTest (TestM reader) e = runReader reader e

type TestFunctions = {
  log :: String -> TestM Unit,
  getUserName :: TestM Name
}

instance LogToScreen TestM where
  log message = do
    functions <- ask
    functions.log message
    pure unit

instance GetUserName TestM where
  getUserName = do
    functions <- ask
    functions.getUserName

spec :: Spec Unit
spec = do
  describe "Three Layer Test" do
    it "program test" do
      let
        logMock = mock $ any :> (pure unit :: TestM Unit)
        functions = { 
          log: fun logMock,
          getUserName: pure (Name "John")
        }

      -- run  
      runTest program functions `shouldEqual` unit

      -- verify
      verifySequence logMock [
        "What is your name?",
        "You name is John"
      ]
