module Test.Gateway.ArticleGatewaySpec where

import Prelude

import Data.Array (cons)
import Driver.ArticleDriver (ArticleDriverType)
import Driver.ArticleESDriver (ArticleESDriverType)
import Gateway.ArticleGateway (findByTitle)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

{-
  whenを実装するにあたり
  いくつになるかわからない引数の値をどっかに保存しておいて、マッチするかどうかの検証に使いたい。
  保存しておくデータ構造としてArrayを考えたがArrayは異なる型を持てない。
  では異なる型の値を束ねて持つにはどうしたらいいか？
  できれば型の情報もほしいのだが。

-}
-- class Mock t where
--   args :: forall a. Array a -> t

-- newtype MockT a = MockT a

-- -- instance sprintfOut :: (Mock a, Mock b) => Mock (b -> a) where
-- --   args acc = (\a -> args (cons a acc))

-- -- when :: 

-- aaa = mock (calcShow :: Int -> Int -> String) `when` (3 5) `returns` "hoge"

{-
  テストが書けなくはないが、everyとか使いたい
-}
spec :: Spec Unit
spec = do
  describe "FindArticleGateway" do
    it "タイトルに紐づくArticleを取得してStateを更新できる" do
      let
        esDriver = {
          findIndexByTitle: \title -> pure if title == "タイトル" then {id: "ID"} else {id: "BadID"}
        } :: ArticleESDriverType
        driver = {
          findById: \id -> pure if id == "ID" then {id: id, title: "期待値のタイトル"} else {id: "", title: "誤ったタイトル"}
        } :: ArticleDriverType
      result <- findByTitle "タイトル" esDriver driver
      result `shouldEqual` {title: "期待値のタイトル"}

    -- it "タイトルに紐づくArticleを取得してStateを更新できる" do
    --   let
    --     -- findIndexByTitleMock = when findIndexByTitle -- `withArgs` "タイトル" `returns` pure {id: "ID"}
    --     z :: Int -> Int -> String
    --     z = when 1 10 # thenReturn "11"
    --     y = z 1 10
    --   --   esDriver = {
    --   --     findIndexByTitle: \title -> pure if title == "タイトル" then {id: "ID"} else {id: "BadID"}
    --   --   } :: ArticleESDriverType
    --   --   driver = {
    --   --     findById: \id -> pure if id == "ID" then {id: id, title: "期待値のタイトル"} else {id: "", title: "誤ったタイトル"}
    --   --   } :: ArticleDriverType
    --   -- result <- findByTitle "タイトル" esDriver driver
    --   y `shouldEqual` 11