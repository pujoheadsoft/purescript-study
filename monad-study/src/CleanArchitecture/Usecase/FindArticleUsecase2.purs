module CleanArchitecture.Usecase.FindArticleUsecase2 where

import Prelude

import CleanArchitecture.Domain.Article (Article, ArticleTitle)
import CleanArchitecture.Driver.ArticleApiDriver2 (createArticleDriverMock)
import CleanArchitecture.Gateway.ArticleGateway2 (ArticleGateway, createArticleGateway)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Foldable (oneOfMap)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Study.Control.Monad.Run.Choose (CHOOSE, runChoose)
import Study.Control.Monad.Run.Except (EXCEPT, runExcept)
import Study.Control.Monad.Run.Reader (READER, runReader)
import Study.Control.Monad.Run.Run (AFF, Run, runBaseAff)
import Type.Row (type (+))

type ArticleUsecase = {
   findArticlesByTitle :: forall r. ArticleTitle -> Run (CHOOSE + AFF + EXCEPT String + r) (Maybe Article)
}

createUsecase :: ArticleGateway -> ArticleUsecase
createUsecase gateway = {
  findArticlesByTitle: \title -> findArticlesByTitle2 title gateway
}

-- CHOOSEを使ってると(Maybe Article) Run r a の a は使うときArrayとなる
findArticlesByTitle2 :: forall r. ArticleTitle -> ArticleGateway -> Run (CHOOSE + AFF + EXCEPT String + r) (Maybe Article)
findArticlesByTitle2 title gateway = do
  ids <- gateway.findArticleIdsByTitle title
  articles <- oneOfMap (\id -> gateway.findArticleById id) ids -- mapしつつ一つに畳み込む
  pure articles

main :: Effect Unit
main = launchAff_ do
  liftEffect $ log "----- Usecase2 Start ------------------"
  let
    driver = createArticleDriverMock {host: "http://article-api"}
    gateway = createArticleGateway driver
    usecase = createUsecase gateway
  result <- usecase.findArticlesByTitle "UsecaseTest2" 
    # runChoose -- CHOOSEのエフェクトを除去 (runChooseの結果は型推論ができなくなるので↓のように::演算子を使って式に対して型注釈を加えると正しい型クラスインスタンスを判断できるようになる)
    # runExcept -- EXCEPTのエフェクトを除去 (Exceptを使うとrunの型がEitherになるので↓のような処理が必要になる)
    # runBaseAff
  case result of
    Left error -> 
      liftEffect $ log error
    Right articles -> do
      liftEffect $ logShow $ catMaybes (articles :: Array (Maybe Article)) -- Chooseを使う場合この型注釈は必要
      liftEffect $ log "----- Usecase2 End --------------------"


------------------------------------------------------------------------------
{-
  同じ副作用を合成しようとするとうまくいかない (READER String + READER Int)
  合成しようとしているところで副作用をはがせば同時に使えるがどうなんだろう
  拡張可能作用を拡張可能レコードで実現してるのがよくないんだろうな
-}
type IndexSearchPort = {
  findByKeyword ::  forall r. String -> Run (AFF + READER String + r) String
}

type News = {title :: String}
type NewsPort = {
  findByIndex :: forall r. String -> Run (AFF + READER Int + r) News
}

search :: forall r. IndexSearchPort -> NewsPort -> Run (AFF + r) News
search indexSearchPort newsPort = do
  index <- indexSearchPort.findByKeyword "" 
          # flip runReader "" -- はがす
  news <- newsPort.findByIndex index
          # flip runReader 10 -- はがす
  pure news