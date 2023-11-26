module Test.TaglessFinal.Gateway.GatewaySpec where

import Prelude

import Control.Monad.Reader (runReaderT)
import Effect.Aff (Aff)
import TaglessFinal.Gateway.Gateway (findByTitle)
import Test.PMock (any, fun, mock, namedMock, namedMockFun, verify, verifySequence, (:>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Tagless Final形式のGatewayのテスト" do
    it "渡したタイトルを元に記事を取得することができる" do
      let        
        functions = { 
          findIdsByTitle: namedMockFun "findIdsByTitle" $ "weather" :> pure@Aff ["id1", "id2"], 
          findById: namedMockFun "findById" $ [
            "id1" :> pure @Aff {id: "id1", title: "title1"},
            "id2" :> pure @Aff {id: "id2", title: "title2"}
          ] 
        }

      result <- runReaderT (findByTitle "weather") functions
      
      result `shouldEqual` [
        { title: "title1" },
        { title: "title2" }
      ]
