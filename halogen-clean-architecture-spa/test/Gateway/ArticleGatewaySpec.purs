module Test.Gateway.ArticleGatewaySpec where

import Prelude

import Driver.ArticleDriver (ArticleDriverType)
import Driver.ArticleESDriver (ArticleESDriverType)
import Gateway.ArticleGateway (findByTitle)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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