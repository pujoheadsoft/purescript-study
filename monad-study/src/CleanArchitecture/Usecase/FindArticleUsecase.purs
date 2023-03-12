module CleanArchitecture.Usecase.FindArticleUsecase where

import Prelude

import CleanArchitecture.Domain.Article (Article, ArticleTitle)
import CleanArchitecture.Driver.ArticleApiDriver (makeArticleDriverMock)
import CleanArchitecture.Driver.Environment (Environment)
import CleanArchitecture.Gateway.ArticleGateway (ArticleGatewayType, makeArticleGateway)
import Data.Array (catMaybes)
import Data.Foldable (oneOfMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Study.Control.Monad.Run.Choose (CHOOSE, runChoose)
import Study.Control.Monad.Run.Reader (READER, runReader)
import Study.Control.Monad.Run.Run (AFF, Run, runBaseAff)
import Type.Row (type (+))

type ArticleUsecaseType = {
   findArticlesByTitle :: forall r. ArticleTitle -> Run (CHOOSE + AFF + READER Environment + r) (Maybe Article)
}

makeUsecaseType :: ArticleGatewayType -> ArticleUsecaseType
makeUsecaseType gateway = {
  findArticlesByTitle: \title -> findArticlesByTitle title gateway
}

-- CHOOSEを使ってると(Maybe Article) Run r a の a は使うときArrayとなる
findArticlesByTitle :: forall r. ArticleTitle -> ArticleGatewayType -> Run (CHOOSE + AFF + READER Environment + r) (Maybe Article)
findArticlesByTitle title gateway = do
  ids <- gateway.findArticleIdsByTitle title
  articles <- oneOfMap (\id -> gateway.findArticleById id) ids -- mapしつつ一つに畳み込む
  pure articles

main :: Effect Unit
main = launchAff_ do
  liftEffect $ log "Usecase Start"
  let
    driver = makeArticleDriverMock -- mock
    gateway = makeArticleGateway driver
    usecase = makeUsecaseType gateway
  articles <- usecase.findArticlesByTitle "TestTitle" 
    # runChoose                                   -- CHOOSEのエフェクトを除去 (runChooseの結果は型推論ができなくなるので↓のように::演算子を使って式に対して型注釈を加えると正しい型クラスインスタンスを判断できるようになる)
    # flip runReader {host: "http://article-api"} -- READERのエフェクトを除去 flipしているのはrunReaderの第一引数がrunだが渡ってくるのは第二引数だから
    # runBaseAff
  liftEffect $ logShow $ catMaybes (articles :: Array (Maybe Article)) -- Chooseを使う場合この型注釈は必要
  liftEffect $ log "Usecase End"
  