module Test.MockSpec3 where

import Prelude

import Component.State (State)
import Control.Monad.State (StateT, runStateT)
import Data.Identity (Identity)
import Domain.Article (Article)
import Effect.Aff (Aff)
import Test.Mock3 (Cons, Param, Verifier, any, mock, verify, verifyCount, (:>), matcher)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Proxyを使って戻りの型を動的に決定できる
spec :: Spec Unit
spec = do
  describe "Mock3のテスト" do
    describe "任意の引数に対して任意の値を返す関数を生成することができる" do
      it "引数が1つの場合" do
        let
          m = mock $ "a" :> 100
        m.fun "a" `shouldEqual` 100

      it "引数が2つの場合" do
        let
          m = mock $ "a" :> 2 :> 1000    

        m.fun "a" 2 `shouldEqual` 1000
        
      it "引数が3つの場合" do
        let
          m = mock $ "a" :> 2 :> true :> 10000
        m.fun "a" 2 true `shouldEqual` 10000

  --   describe "期待する引数でない引数で呼び出した場合failになる" do
  --     it "引数が1つの場合" do
  --       let
  --         m = mock $ 1 :> 100
  --       m.fun 1 `shouldEqual` 100
  --       --m.fun 2 `shouldEqual` 100
  --       --expectErrorF (\_ -> m.fun 2)
  --     it "引数が2つの場合" do
  --       let
  --         m = mock $ 1 :> "a" :> 100
  --       m.fun 1 "a" `shouldEqual` 100
  --       --m.fun 1 "b" `shouldEqual` 100
  --     it "引数が3つの場合" do
  --       let
  --         m = mock $ 1 :> "a" :> true :> 100
  --       m.fun 1 "a" true `shouldEqual` 100
  -- --      m.fun 1 "b" true `shouldEqual` 100

    describe "特定の引数で呼び出されたことを検証することができる" do
      it "引数が1つの場合" do
        let
          m = mock $ 9 :> 100
          _ = m.fun 9
        verify m 9

      it "引数が2つの場合" do
        let
          m = mock $ 9 :> false :> 100
          _ = m.fun 9 false
        verify m $ 9 :> false

      it "引数が3つの場合" do
        let
          m = mock $ 9 :> false :> "hoge" :> 100
          _ = m.fun 9 false "hoge"
        verify m $ 9 :> false :> "hoge"

      -- it "一度も呼び出さない状態で検証をするとfailになる" do
      --   let
      --     m = mock $ whenCalledWith 1 `returns` 100
      --   expectError $ verify m.mock
      
      it "Monadを返すことができる1" do
        let
          m = mock $ 1 :> (pure "hoge" :: Identity String)
          _ = (m.fun :: (Int -> Identity String)) 1
        verify m 1
      
      it "Monadを返すことができる2" do
        let
          -- Monad m のようにする場合、いまどのMonadで動いてるのかわからないといけない(mは駄目で、ちゃんと指定しないといけない)
          findByTitleMock = mock $ "古いタイトル" :> (pure { title: "新しいタイトル" } :: Aff Article)

        result <- findByTitleMock.fun "古いタイトル"

        result `shouldEqual` {title: "新しいタイトル"}
        
        verify findByTitleMock "古いタイトル"
      
      it "Monadを返すことができる3" do
        let
          updateMock = mock $ "新しいtitle" :> (pure unit :: StateT State Aff Unit)
        _ <- runStateT (updateMock.fun "新しいtitle") {article: {title: "Dummy"}} 
        verify updateMock "新しいtitle"

    describe "呼び出し回数を検証することができる" do
      it "呼び出された回数を検証することができる(0回)" do
        let
          m = mock $ 1 :> 100
        verifyCount m 1 0

      it "呼び出された回数を検証することができる(複数回)" do
        let
          m = mock $ 1 :> 2 :> 100
          _ = m.fun 1 2
          _ = m.fun 1 2
          _ = m.fun 1 2
        verifyCount m (1 :> 2) 3
    
    describe "一つのMockで複数の引数の組み合わせに対応できる" do
      describe "任意の値を返すことができる" do
        it "引数が1つの場合" do
          let
            m = mock [1 :> "r1",
                      2 :> "r2"]
          
          m.fun 1 `shouldEqual` "r1"
          m.fun 2 `shouldEqual` "r2"

        it "引数が2つの場合" do
          let
            m = mock [1 :> "2" :> "r1",
                      2 :> "3" :> "r2"]
          
          m.fun 1 "2" `shouldEqual` "r1"
          m.fun 2 "3" `shouldEqual` "r2"

      describe "検証することができる" do
        it "引数が1つの場合" do
          let
            m = mock [1 :> "r1",
                      2 :> "r2"]
          
            _ = m.fun 1
            _ = m.fun 2
          verify m 1
          verify m 2

        it "引数が2つの場合" do
          let
            m = mock [1 :> "2" :> "r1",
                      2 :> "3" :> "r2"]
            _ = m.fun 1 "2"
            _ = m.fun 2 "3"

          verify m (1 :> "2")
          verify m (2 :> "3")

      it "呼び出された回数を検証することができる(複数回)" do
        let
          m = mock [1 :> "2" :> "r1",
                    2 :> "3" :> "r2"]
          _ = m.fun 1 "2"
          _ = m.fun 2 "3"
          _ = m.fun 1 "2"

        verifyCount m (1 :> "2") 2

    describe "Matcher" do
      describe "any" do
        it "引数が1つの場合" do
          let
            m = mock $ (any :: Param String) :> "Expected"
          
          m.fun "" `shouldEqual` "Expected"
          m.fun "1" `shouldEqual` "Expected"
          m.fun "a" `shouldEqual` "Expected"

        it "引数が2つの場合" do
          let
            m = mock $ (any :: Param String) :> (any :: Param Int) :> "Expected"
          
          m.fun "" 0    `shouldEqual` "Expected"
          m.fun "1" 9   `shouldEqual` "Expected"
          m.fun "a" 100 `shouldEqual` "Expected"

        it "引数が2つの場合2" do
          let
            m = mock $ "a" :> (any :: Param Int) :> "Expected"
          
          m.fun "a" 0   `shouldEqual` "Expected"
          m.fun "a" 9   `shouldEqual` "Expected"
          m.fun "a" 100 `shouldEqual` "Expected"

      describe "any verify" do
        it "引数が1つの場合" do
          let
            m = mock $ (any :: Param String) :> "Expected"
          
            _ = m.fun "foo"
            _ = m.fun "bar"
          
          verify m "foo"
          verify m "bar"
          verify m (any :: Param String)

        it "引数が2つの場合" do
          let
            m = mock $ (any :: Param String) :> (any :: Param Int) :> "Expected"
          
            _ = m.fun "foo" 0    
            _ = m.fun "bar" 9

          verify m ("foo" :> 0)
          verify m ("bar" :> 9)
          verify m ((any :: Param String) :> 0)
          verify m ((any :: Param String) :> 9)

    describe "custom matcher" do
      it "引数が1つの場合" do
        let
          m = mock $ matcher (\v -> v > 10) "> 10" :> "Expected"

        m.fun 11 `shouldEqual` "Expected"

        verify m (matcher (\v -> v > 10) "> 10")

    describe "Cons" do
      describe "Show" do
        it "arg2" do
          show (10 :> true) `shouldEqual` "10, true"
        it "arg3" do
          show ("1" :> false :> [3, 4]) `shouldEqual` "\"1\", false, [3,4]"
      describe "Eq" do
        it "arg2" do
          (1 :> "2") `shouldEqual` (1 :> "2")
        it "arg3" do
          ("1" :> false :> [3, 4]) `shouldEqual` ("1" :> false :> [3, 4])
