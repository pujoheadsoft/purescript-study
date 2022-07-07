module Test.CleanArchitecture.Usecase.UserUsecase where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- findUserById :: UserId -> Effect Unit
-- findUserById id = log ""

-- -- このfindUserByIdで使う2つのportを簡単にmockに差し替えたい

-- spec :: Spec Unit
-- spec = do
--   describe "UserUsecase" do
--     it "指定したIDのユーザ情報を表示することができる" do
--       findUserById "" `shouldEqual` pure unit