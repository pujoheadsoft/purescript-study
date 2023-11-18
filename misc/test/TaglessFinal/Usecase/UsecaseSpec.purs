module Test.TaglessFinal.Usecase.UsecaseSpec where

import Prelude

import Control.Monad.Reader (runReaderT)
import Effect.Aff (Aff)
import TaglessFinal.Domain.Article (Article)
import TaglessFinal.Usecase.Usecase (execute)
import Test.PMock (any, fun, mock, verify, (:>))
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec = do
  describe "Tagless Final形式のUsecaseのテスト" do
    it "渡したタイトルを元に記事を取得して更新することができる" do
      let
        articles = [
           {title: "weather news"}
          ,{title: "weather report"}
        ]
        findByTitleMock = mock $ "weather" :> pure@Aff articles
        
        updateMock = mock $ any@(Array Article) :> pure@Aff unit

        functions = { findByTitle: fun findByTitleMock, update: fun updateMock }

      _ <- runReaderT (execute "weather") functions
      
      verify updateMock articles
