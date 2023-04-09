module Test.MockSpec where

import Prelude

import Component.State (State)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.State (class MonadState, StateT, runStateT)
import Data.Identity (Identity)
import Domain.Article (Article)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Test.Mock (class MockBuilder, Mock, fun, mock, thenReturn, verify, verifyCount, (:))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (expectError, shouldEqual)

spec :: Spec Unit
spec = do
  pending ""
  -- describe "任意の引数に対して任意の値を返す関数を生成することができる" do
  --   it "引数が1つの関数を生成できる" do
  --     let
  --       m = mock "a" `thenReturn` 100
  --     (fun m) "a" `shouldEqual` 100
  
  -- -- describe "期待する引数でない引数で呼び出した場合failになる" do
  -- --   it "引数が1つの場合" do
  -- --     let
  -- --       m = mock "a" `thenReturn` 100
  -- --     (fun m) "b" `shouldEqual` 100
  -- --   it "引数が2つの場合" do
  -- --     let
  -- --       m = mock (1 : "a") `thenReturn` 100
  -- --     (fun m) 1 "b" `shouldEqual` 100

  -- describe "特定の引数で呼び出されたことを検証することができる" do
  --   it "mock" do
  --     let
  --       m = mock (1 : "2" : 3) `thenReturn` 100
  --       _ = (fun m) 1 "2" 3
  --     verify m

  --   it "一度も呼び出さない状態で検証をするとfailになる" do
  --     let
  --       m = mock 1 `thenReturn` 100
  --     expectError $ verify m
    
  --   it "Monadを返すことができる1" do
  --     let
  --       m :: Mock (Int -> Identity String)
  --       m = mock 1 `thenReturn` pure "hoge"

  --       _ = (fun m) 1
  --     verify m
    
  --   it "Monadを返すことができる2" do
  --     let
  --       findByTitleMock :: forall m. Monad m => Mock (String -> m Article)
  --       findByTitleMock = mock "古いタイトル" `thenReturn` pure { title: "新しいタイトル" }
  --     result <- (fun findByTitleMock) "古いタイトル"
  --     result `shouldEqual` {title: "新しいタイトル"}
  --     -- Monad m のようにする場合、いまどのMonadで動いてるのかわからないといけない(mは駄目で、ちゃんと指定しないといけない)
  --     verify (findByTitleMock :: Mock (String -> Aff Article))
    
  --   it "Monadを返すことができる3" do
  --     let
  --       updateMock :: forall m. MonadState State m => Mock (String -> m Unit)
  --       updateMock = mock "新しいtitle" `thenReturn` pure unit
  --       f = fun (updateMock :: Mock (String -> StateT State Aff Unit))
  --     _ <- runStateT (f "新しいtitle") {article: {title: "Dummy"}} 
  --     verify (updateMock :: Mock (String -> StateT State Aff Unit))

  -- describe "呼び出し回数を検証することができる" do
  --   it "呼び出された回数を検証することができる(0回)" do
  --     let
  --       m = mock 1 `thenReturn` 100
  --     verifyCount m 0

    -- it "呼び出された回数を検証することができる(複数回)" do
    --   let
    --     m = mock (1 : 2)`thenReturn` 100
    --     _ = (fun m) 1 2
    --     _ = (fun m) 1 2
    --   verifyCount m 2
  