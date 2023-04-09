module Test.MockSpec2 where

import Prelude

import Component.State (State)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.State (class MonadState, StateT, runStateT)
import Data.Identity (Identity)
import Domain.Article (Article)
import Effect.Aff (Aff)
import Effect.Exception (Error)
import Test.Mock2 (class MockBuilder, Cons, Definition, Mock, calledWith, fun, mock, returns, verify, verifyCount, (:), (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)


expectErrorF :: forall m f. MonadError Error m => f -> m Unit
expectErrorF f = expectError $ ((\_ -> let 
    _ = f 
  in pure unit) :: forall em. MonadError Error em => Unit -> em Unit) unit

spec :: Spec Unit
spec = do
  describe "任意の引数に対して任意の値を返す関数を生成することができる" do
    it "引数が1つの関数を生成できる" do
      let
        m = mock $ calledWith "a" `returns` 100
      (fun m) "a" `shouldEqual` 100
  
  -- describe "期待する引数でない引数で呼び出した場合failになる" do
  --   it "引数が1つの場合" do
  --     let
  --       m = mock $ 1 :> 100
  --       f = (fun m)
  --     f 1 `shouldEqual` 100
  --     expectErrorF (\_ -> f 2)
    -- it "引数が2つの場合" do
    --   let
    --     m = mock (1 : "a") `returns` 100
    --   (fun m) 1 "b" `shouldEqual` 100

  describe "特定の引数で呼び出されたことを検証することができる" do
    it "mock" do
      let
        m = mock $ calledWith (1 : "2" : 3) `returns` 100
        _ = (fun m) 1 "2" 3
      verify m

    it "一度も呼び出さない状態で検証をするとfailになる" do
      let
        m = mock $ calledWith 1 `returns` 100
      expectError $ verify m
    
    it "Monadを返すことができる1" do
      let
        m :: Mock (Int -> Identity String)
        m = mock $ calledWith 1 `returns` pure "hoge"

        _ = (fun m) 1
      verify m
    
    it "Monadを返すことができる2" do
      let
        findByTitleMock :: forall m. Monad m => Mock (String -> m Article)
        findByTitleMock = mock $ calledWith "古いタイトル" `returns` pure { title: "新しいタイトル" }
      result <- (fun findByTitleMock) "古いタイトル"
      result `shouldEqual` {title: "新しいタイトル"}
      -- Monad m のようにする場合、いまどのMonadで動いてるのかわからないといけない(mは駄目で、ちゃんと指定しないといけない)
      verify (findByTitleMock :: Mock (String -> Aff Article))
    
    it "Monadを返すことができる3" do
      let
        updateMock :: forall m. MonadState State m => Mock (String -> m Unit)
        updateMock = mock $ calledWith "新しいtitle" `returns` pure unit
        f = fun (updateMock :: Mock (String -> StateT State Aff Unit))
      _ <- runStateT (f "新しいtitle") {article: {title: "Dummy"}} 
      verify (updateMock :: Mock (String -> StateT State Aff Unit))

  describe "呼び出し回数を検証することができる" do
    it "呼び出された回数を検証することができる(0回)" do
      let
        m = mock $ calledWith 1 `returns` 100
      verifyCount m 0

    it "呼び出された回数を検証することができる(複数回)" do
      let
        m = mock $ calledWith (1 : 2) `returns` 100
        _ = (fun m) 1 2
        _ = (fun m) 1 2
      verifyCount m 2
  
  {-
    m = mock [(1 : 2), (2 : 3)] `returns` ["r1", "r2"]
    multiMock [(1 : 2) `returns` r1, (2 : 3) `returns` r2]
    do
      mock (1 : 2) `returns` "r1"
      mock (2 : 3) `returns` "r2"
  -}
  describe "一つのMockで複数の引数の組み合わせに対応できる" do
    it "引数が1つの場合" do
      let
        m :: Mock (Int -> String)
        m = mock [1 :> "r1",
                  2 :> "r2"]
       
      (fun m) 1 `shouldEqual` "r1"
      (fun m) 2 `shouldEqual` "r2"

