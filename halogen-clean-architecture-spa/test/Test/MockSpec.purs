module Test.MockSpec where

import Prelude
import Component.State (State)
import Control.Monad.State (class MonadState, StateT, runStateT)
import Data.Identity (Identity)
import Domain.Article (Article)
import Effect.Aff (Aff)
import Test.Mock (Mock, fun, mock, thenReturn, verify, (:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec = do
  describe "Mock Test" do
    it "mock" do
      let
        m = mock (1 : "2" : 3) `thenReturn` 100
      verify m
    
    it "Monadを返すことができる1" do
      let
        m :: Mock (Int -> Identity String)
        m = mock 1 `thenReturn` pure "hoge"
      verify m
    
    it "Monadを返すことができる2" do
      let
        findByTitleMock :: forall m. Monad m => Mock (String -> m Article)
        findByTitleMock = mock "古いタイトル" `thenReturn` pure { title: "新しいタイトル" }
      result <- (fun findByTitleMock) "古いタイトル"
      result `shouldEqual` {title: "新しいタイトル"}
      -- Monad m のようにする場合、いまどのMonadで動いてるのかわからないといけない(mは駄目で、ちゃんと指定しないといけない)
      verify (findByTitleMock :: Mock (String -> Aff Article))
    
    it "Monadを返すことができる3" do
      let
        updateMock :: forall m. MonadState State m => Mock (String -> m Unit)
        updateMock = mock "新しいtitle" `thenReturn` pure unit
        f = fun (updateMock :: Mock (String -> StateT State Aff Unit))
      _ <- runStateT (f "新しいtitle") {article: {title: "Dummy"}} 
      verify (updateMock :: Mock (String -> StateT State Aff Unit))
