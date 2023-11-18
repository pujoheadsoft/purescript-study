module Test.TaglessFinal.Usecase.UsecaseSpec where

import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (class MonadState, StateT, execStateT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import TaglessFinal.Domain.Article (Article)
import TaglessFinal.Proposal (displayTrialStatus, findUserOptionByUserId, findUserWithDisplayTrialStatus)
import TaglessFinal.State.State (State)
import TaglessFinal.Usecase.Usecase (execute)
import Test.PMock (any, fun, mock, mockFun, verify, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Tagless Final形式のUsecaseのテスト" do
    it "渡したタイトルを元にArticleを取得して、StateをArticleのタイトルで更新することができる" do

      let
        findByTitleMock = mock $ "title" :> pure@(StateT State Aff) {title: "article title"}
        
        updateMock = mock $ any@String :> pure@(StateT State Aff) unit

        functions = { findByTitle: fun findByTitleMock, update: fun updateMock }

      _ <- runReaderT (execute "title") functions
          # flip execStateT {article: {title: ""}}
      
      verify updateMock "article title"