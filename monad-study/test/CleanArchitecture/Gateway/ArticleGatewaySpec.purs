module Test.CleanArchitecture.Gateway.ArticleGatewaySpec where

import Prelude

import CleanArchitecture.Gateway.ArticleGateway as Gateway
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "UserUsecase" do
    it "指定したIDのユーザ情報を表示することができる" do
      let
        expected = [{title: "title1", body: "body2", author: "author3"}]
      Gateway.findArticlesByTitle "" `shouldEqual` expected