module Test.Study.Control.Monad.Transformer.ContSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Study.Control.Monad.Transformer.Cont (Cont, ContT, callCC, cont, runCont, runContT)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Config = { debug :: Boolean }

spec :: Spec Unit
spec = do
  describe "Contのテスト(Transformer版)" do
    describe "runCont" do
      it "継続を渡すことができる" do
        let
          -- 加算の結果を継続に渡すCont
          addCont :: forall r. Int -> Int -> Cont r Int
          addCont x y = cont (\f -> f (x + y))

          -- 乗算の結果を継続に渡すCont
          multiplyCont :: forall r. Int -> Int -> Cont r Int
          multiplyCont x y = cont (\f -> f (x * y))

          -- 2乗にしたもの同士を加算する計算の結果を継続に渡すCont
          calc :: forall r. Int -> Int -> Cont r Int
          calc x y = do
            v1 <- multiplyCont x x
            v2 <- multiplyCont y y
            addCont v1 v2

        -- 継続の処理は文字列化をする`show`とする
        runCont (calc 2 3) show `shouldEqual` "13"
    
    describe "callCC" do
      describe "現在の継続を利用することができる" do
        it "渡した継続を使わない場合、処理は最後まで進む" do
          runCont (callCC (\_ -> pure 1)) identity `shouldEqual` 1

        it "渡した継続を使う場合は、そこで処理が中断される" do
          let
            f = callCC (\c -> do 
              _ <- c 10 -- 継続を呼び出す
              pure 100
            )
          -- 最終的には100を返しているが、実際の結果は10となる
          runCont f show `shouldEqual` "10"

    describe "ContT" do
      it "モナドを合成できる" do
        let
          -- ContとMaybeを合成
          m :: forall r . ContT r Maybe String
          m = pure "value"
        runContT m pure `shouldEqual` (Just "value")