module Test.MockSpec3 where

import Prelude

import Component.State (State)
import Control.Monad.Except (class MonadError)
import Control.Monad.State (StateT, runStateT)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Domain.Article (Article)
import Effect.Aff (Aff, Error)
import Test.Mock3 (Param, any, matcher, mock, runRuntimeThrowableFunction, verify, verifyCount, (:>))
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (expectError, shouldEqual)

{-
  テストのパターン
  引数 1 - 10、multi 1 - 10
  に対して
  ・関数を期待する引数で呼ぶと期待する値が返る
  ・関数を期待する引数以外で呼ぶとfail
  ・特定の引数で呼ばれたか検証
  ・特定の引数で特定回数呼ばれたか検証
  ・期待する引数をmatcherにできる(組み込みのものと、カスタムのもの)
  ・検証する引数をmatcherにできる(組み込みのものと、カスタムのもの)

-}

type Fixture mock r m = {
  name :: String,
  create :: Unit -> mock,
  execute :: mock -> r,
  executeFailed :: Maybe (mock -> r),
  expected :: r,
  verifyMock :: mock -> m Unit,
  verifyCount :: mock -> Int -> m Unit,
  verifyFailed :: mock -> m Unit
}

{-
  MonadThrow と MonadError がある
-}

-- mock test template
mockTest :: forall mock m g r. Monad m => Eq r => Show r => MonadError Error g => Fixture mock r g -> SpecT g Unit m Unit
mockTest f = describe f.name do
  it "設定した引数で実行すると設定した値を返すことができる" do
    let m = f.create unit
    f.execute m `shouldEqual` f.expected

  it "設定した引数で実行しないと失敗する" do
    case f.executeFailed of
      Just func -> let m = f.create unit
        in expectError $ runRuntimeThrowableFunction (\_ -> func m)
      Nothing -> pure unit

  it "指定した引数で呼び出されたかどうかを検証できる" do
    let 
      m = f.create unit
      _ = f.execute m
    f.verifyMock m

  it "指定した引数で呼び出されていない場合は検証に失敗する" do
    let 
      m = f.create unit
      _ = f.execute m
    expectError $ f.verifyFailed m

  it "指定した引数で呼び出された回数を検証できる(0回)" do
    let m = f.create unit
    f.verifyCount m 0

  it "指定した引数で呼び出された回数を検証できる(複数回)" do
    let 
      m = f.create unit
      _ = f.execute m
      _ = f.execute m
      _ = f.execute m
    f.verifyCount m 3

spec :: Spec Unit
spec = do
  describe "Mock3のテスト" do
    mockTest {
      name: "引数が1つの場合", 
      create: \_ -> mock $ "1" :> 1,
      expected: 1, 
      execute: \m -> m.fun "1",
      executeFailed: Just \m -> m.fun "2",
      verifyMock: \m -> verify m "1",
      verifyCount: \m c -> verifyCount m c "1",
      verifyFailed: \m -> verify m "2"
    }

    mockTest {
      name: "引数が2つの場合", 
      create: \_ -> mock $ 100 :> "1" :> true,
      expected: true, 
      execute: \m -> m.fun 100 "1",
      executeFailed: Just \m -> m.fun 100 "2",
      verifyMock: \m -> verify m $ 100 :> "1",
      verifyCount: \m c -> verifyCount m c $ 100 :> "1",
      verifyFailed: \m -> verify m $ 100 :> "2"
    }

    mockTest {
      name: "引数が3つの場合", 
      create: \_ -> mock $ 100 :> "1" :> true :> 11.1,
      expected: 11.1, 
      execute: \m -> m.fun 100 "1" true,
      executeFailed: Just \m -> m.fun 100 "1" false,
      verifyMock: \m -> verify m $ 100 :> "1" :> true,
      verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true,
      verifyFailed: \m -> verify m $ 100 :> "1" :> false
    }

    mockTest {
      name: "引数が4つの場合", 
      create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2],
      expected: [1, 2], 
      execute: \m -> m.fun 100 "1" true 11.1,
      executeFailed: Just \m -> m.fun 100 "1" true 11.0,
      verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1,
      verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1,
      verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.0
    }

    mockTest {
      name: "引数が5つの場合", 
      create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
      expected: {name: "Name"}, 
      execute: \m -> m.fun 100 "1" true 11.1 [1, 2],
      executeFailed: Just \m -> m.fun 100 "1" true 11.1 [1, 3],
      verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2],
      verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2],
      verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [2, 2]
    }

    mockTest {
      name: "引数が6つの場合", 
      create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
      expected: 20, 
      execute: \m -> m.fun 100 "1" true 11.1 [1, 2] {name: "Name"},
      executeFailed: Just \m -> m.fun 100 "1" true 11.1 [1, 3] {name: "Nam"},
      verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
      verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"},
      verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Nome"}
    }

    mockTest {
      name: "引数が7つの場合", 
      create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
      expected: "X", 
      execute: \m -> m.fun 100 "1" true 11.1 [1, 2] {name: "Name"} 20,
      executeFailed: Just \m -> m.fun 100 "1" true 11.1 [1, 3] {name: "Name"} 21,
      verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
      verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20,
      verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 19
    }

    mockTest {
      name: "引数が8つの場合", 
      create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
      expected: false, 
      execute: \m -> m.fun 100 "1" true 11.1 [1, 2] {name: "Name"} 20 "X",
      executeFailed: Just \m -> m.fun 100 "1" true 11.1 [1, 3] {name: "Name"} 20 "Y",
      verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
      verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X",
      verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "Z"
    }

    mockTest {
      name: "引数が9つの場合", 
      create: \_ -> mock $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false :> 0.1,
      expected: 0.1, 
      execute: \m -> m.fun 100 "1" true 11.1 [1, 2] {name: "Name"} 20 "X" false,
      executeFailed: Just \m -> m.fun 100 "1" true 11.1 [1, 3] {name: "Name"} 20 "X" true,
      verifyMock: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
      verifyCount: \m c -> verifyCount m c $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> false,
      verifyFailed: \m -> verify m $ 100 :> "1" :> true :> 11.1 :> [1, 2] :> {name: "Name"} :> 20 :> "X" :> true
    }

    describe "Multi Mock" do
      mockTest {
        name: "引数が1つの場合", 
        create: \_ -> mock $ [
          "1" :> 10, 
          "2" :> 20
        ],
        expected: [
          10, 
          20
        ], 
        execute: \m -> [
          m.fun "1", m.fun "2"
        ],
        executeFailed: Just \m -> [ m.fun "3" ],
        verifyMock: \m -> do 
          verify m "1"
          verify m "2"
        ,
        verifyCount: \m c -> do
          verifyCount m c "1"
          verifyCount m c "2"
        ,
        verifyFailed: \m -> verify m "3"
      }

      mockTest {
        name: "引数が2つの場合", 
        create: \_ -> mock $ [
          "1" :> 10 :> true, 
          "2" :> 20 :> false
        ],
        expected: [
          true, 
          false
        ], 
        execute: \m -> [
          m.fun "1" 10, 
          m.fun "2" 20
        ],
        executeFailed: Just \m -> [ m.fun "1" 30 ],
        verifyMock: \m -> do 
          verify m $ "1" :> 10
          verify m $ "2" :> 20
        ,
        verifyCount: \m c -> do
          verifyCount m c $ "1" :> 10
          verifyCount m c $ "2" :> 20
        ,
        verifyFailed: \m -> verify m $ "1" :> 30
      }

      mockTest {
        name: "引数が3つの場合", 
        create: \_ -> mock $ [
          "1" :> 10 :> true  :> "a1", 
          "2" :> 20 :> false :> "a2"
        ],
        expected: [
          "a1", 
          "a2"
        ], 
        execute: \m -> [
          m.fun "1" 10 true, 
          m.fun "2" 20 false
        ],
        executeFailed: Just \m -> [ m.fun "1" 10 false ],
        verifyMock: \m -> do 
          verify m $ "1" :> 10 :> true
          verify m $ "2" :> 20 :> false
        ,
        verifyCount: \m c -> do
          verifyCount m c $ "1" :> 10 :> true
          verifyCount m c $ "2" :> 20 :> false
        ,
        verifyFailed: \m -> verify m $ "1" :> 10 :> false
      }

    mockTest {
      name: "任意の引数が1つの場合", 
      create: \_ -> mock $ (any :: Param String) :> 11,
      expected: 11, 
      execute: \m -> m.fun "1234",
      executeFailed: Nothing,
      verifyMock: \m -> verify m (any :: Param String),
      verifyCount: \m c -> verifyCount m c (any :: Param String),
      verifyFailed: \m -> verify m "not called param"
    }


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


    describe "MonadのMock" do
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
