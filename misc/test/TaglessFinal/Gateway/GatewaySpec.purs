module Test.TaglessFinal.Gateway.GatewaySpec where

import Prelude

import Control.Monad.Reader (runReaderT)
import Effect.Aff (Aff)
import TaglessFinal.Gateway.Gateway (findByTitle)
import Test.PMock (any, fun, mock, verify, verifySequence, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Tagless Final形式のGatewayのテスト" do
    it "渡したタイトルを元に記事を取得することができる" do
      let
        findIdsByTitleMock = mock $ "weather" :> pure@Aff ["id1", "id2"]
        
        findByIdMock = mock $ [
          "id1" :> pure @Aff {id: "id1", title: "title1"},
          "id2" :> pure @Aff {id: "id2", title: "title2"}
        ]

        functions = { 
          findIdsByTitle: fun findIdsByTitleMock, 
          findById: fun findByIdMock 
        }

      result <- runReaderT (findByTitle "weather") functions
      
      result `shouldEqual` [
        { title: "title1" },
        { title: "title2" }
      ]
