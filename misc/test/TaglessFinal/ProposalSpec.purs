module Test.TaglessFinal.ProposalSpec where

import Prelude

import Control.Monad.Reader (runReaderT)
import TaglessFinal.Proposal (displayTrialStatus, findUserOptionByUserId, findUserWithDisplayTrialStatus)
import Test.PMock (any, fun, mock, mockFun, verify, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Tagless Final With Monad Transformer Spec (案)" do
    describe "UserRepositryのテスト" do
      it "指定したIDのユーザーのオプションを取得することができる" do
        let
          mockFunctions = {
            findUserById: mockFun $ "userId" :> {id: "userId", name: "userName", optionId: "optionId"},
            findOptionById: mockFun $ "optionId" :> {optionId: "optionId", isTrial: true}
          }

        option <- findUserOptionByUserId "userId"
                  # flip runReaderT mockFunctions 

        option `shouldEqual` {optionId: "optionId", isTrial: true}

    describe "Presenterのテスト" do
      it "トライアルユーザーかどうかを出力する - トライアルユーザーの場合" do
        let
          displayMock = mock $ any@String :> unit
          functions = {
            display: fun displayMock
          }

        displayTrialStatus {optionId: "optionId", isTrial: true} 
          # flip runReaderT functions

        verify displayMock "This User is Trial Account."

      it "トライアルユーザーかどうかを出力する - トライアルユーザでない場合" do
        let
          displayMock = mock $ any@String :> unit
          functions = {
            display: fun displayMock
          }

        displayTrialStatus {optionId: "optionId", isTrial: false}
          # flip runReaderT functions

        verify displayMock "This User is Premium Account."

    describe "UserRepository + Presenterのテスト" do
      it "指定したIDのユーザーが、トライアルユーザーかどうかを出力する - トライアルユーザーの場合" do
        let
          displayMock = mock $ any@String :> unit
          functions1 = {
            findUserById: mockFun $ "userId" :> {id: "userId", name: "userName", optionId: "optionId"},
            findOptionById: mockFun $ "optionId" :> {optionId: "optionId", isTrial: true},
            display: fun displayMock
          }

        findUserWithDisplayTrialStatus "userId" 
          # flip runReaderT functions1

        verify displayMock "This User is Trial Account."