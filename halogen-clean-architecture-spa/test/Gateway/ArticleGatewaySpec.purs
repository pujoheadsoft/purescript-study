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
          findIndexByTitle: \title -> pure {id: title}
        } :: ArticleESDriverType
        driver = {
          findById: \id -> pure {id: id, title: id}
        } :: ArticleDriverType
      result <- findByTitle "新しいタイトル" esDriver driver
      result `shouldEqual` {title: "新しいタイトル"}