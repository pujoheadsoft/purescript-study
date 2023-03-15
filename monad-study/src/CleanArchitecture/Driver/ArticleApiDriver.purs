module CleanArchitecture.Driver.ArticleApiDriver where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import CleanArchitecture.Driver.ArticleJson (ArticleIndexJson, ArticleJson)
import CleanArchitecture.Driver.Environment (Environment)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, intercalate, length)
import Data.Either (Either(..))
import Data.Functor.Variant (on)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Simple.JSON (readJSON)
import Study.Control.Monad.Run.Reader (READER, ask, runReader)
import Study.Control.Monad.Run.Run (AFF, Run, interpret, lift, liftAff, runBaseAff, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data ArticleDriver a = 
   FindArticleIndicesByTitle String (Array ArticleIndexJson -> a)
 | FindArticlesByIds (Array String) (Array ArticleJson -> a)
 | FindArticleById String (Maybe ArticleJson -> a)

derive instance functorArticleDriver :: Functor ArticleDriver
_articleDriver = Proxy :: Proxy ("articleDriver")
type ARTICLE_DRIVER r = (articleDriver :: ArticleDriver | r)

-----------------------------------------------------------------------------------------------------
{-
  Runを返すだけのやつら
-}
findArticleIndicesByTitle :: forall r. String -> Run (ARTICLE_DRIVER + r) (Array ArticleIndexJson)
findArticleIndicesByTitle title = lift _articleDriver $ FindArticleIndicesByTitle title identity

findArticlesByIds :: forall r. Array String -> Run (ARTICLE_DRIVER + r) (Array ArticleJson)
findArticlesByIds ids = lift _articleDriver $ FindArticlesByIds ids identity

findArticleById :: forall r. String -> Run (ARTICLE_DRIVER + r) (Maybe ArticleJson)
findArticleById id = lift _articleDriver $ FindArticleById id identity

findArticlesByTitle :: forall r. String -> Run (ARTICLE_DRIVER + r) (Array ArticleJson)
findArticlesByTitle title = do
  indexes <- findArticleIndicesByTitle title
  articles <- findArticlesByIds $ (\index -> index.id) <$> indexes
  pure articles

-----------------------------------------------------------------------------------------------
{-
  実際の処理
-}
_findArticleIndicesByTitle :: String -> String -> Aff (Array ArticleIndexJson)
_findArticleIndicesByTitle host title = do
  res <- AX.get ResponseFormat.string $ host <> "/articles?title" <> title
  case res of
    Left err -> do
      --pure $ Left $ "GET /api response failed to decode: " <> printError err
      pure []
    Right response -> do
      case readJSON response.body of
        Left e -> pure [] -- pure $ Left $ show e -- do log $ "Can't parse JSON. " <> show e
        Right (r :: Array ArticleIndexJson) -> pure r -- pure $ Right r -- do log $ "article id is: " <> show r.id

_findArticlesByIds ∷ String -> Array String -> Aff (Array ArticleJson)
_findArticlesByIds host ids = do
  responses <- parTraverse (_findArticleById host) ids
  pure (catMaybes responses)

_findArticleById :: String -> String -> Aff (Maybe ArticleJson)
_findArticleById host id = do
  res <- AX.get ResponseFormat.string $ host <> "/articles/" <> id
  case res of
    Left err -> pure Nothing
    Right response -> do
      case readJSON response.body of
        Left e -> pure Nothing
        Right (r :: ArticleJson) -> pure $ Just r

-------------------------------------------------------------------------------------------
{-
  handler関数
-}
-- 実値を返すhandler
handleDriver :: forall r. String -> ArticleDriver ~> Run (AFF + r)
handleDriver h d = case d of
  FindArticleIndicesByTitle title next -> do
    indices <- liftAff $ _findArticleIndicesByTitle h title
    pure $ next indices
  FindArticlesByIds ids next -> do
    article <- liftAff $ _findArticlesByIds h ids
    pure $ next article
  FindArticleById id next -> do
    article <- liftAff $ _findArticleById h id
    pure $ next article

-- 固定値を返すhandler(mock)
-- handlerをこいつで上書きできればテストで差し替えられる
handleDriverMock :: forall r. String -> ArticleDriver ~> Run (AFF + r)
handleDriverMock h d = case d of
  FindArticleIndicesByTitle title next -> pure $ next [
      {id: "[ID1] 一致した条件 title:" <> title <> ", host:" <> h},
      {id: "[ID2] 一致した条件 title:" <> title <> ", host:" <> h}
  ]
  FindArticlesByIds ids next -> pure $ next $ [{title: intercalate "," ids, body: "body", author: "author"}]
  FindArticleById _ next -> pure $ next $ Just {title: "", body: "", author: ""}

-------------------------------------------------------------------------------------------
{-
  Driverのエフェクトを除去したRunを返す
  (Driverの処理を実行でき、かつ型としてARTICLE_DRIVERが取り除かれた AFF + rのRunを返す)
-}
runDriver :: forall r. String -> Run (AFF + ARTICLE_DRIVER + r) ~> Run (AFF + r)
runDriver h r = interpret (on _articleDriver (handleDriver h) send) r

-- mock用
runDriverWithMock :: forall r. String -> Run (AFF + ARTICLE_DRIVER + r) ~> Run (AFF + r)
runDriverWithMock h r = interpret (on _articleDriver (handleDriverMock h) send) r

-------------------------------------------------------------------------------------------
{-
  色々な処理を実行するRunを返すやつら
  基本的に runDriver に処理対象の Run を渡しているだけ。
-}
runFindArticleIndicesByTitle :: forall r. String -> Run (AFF + READER Environment + r) (Array ArticleIndexJson)
runFindArticleIndicesByTitle title = do
  env <- ask
  runDriver env.host $ findArticleIndicesByTitle title

runFindArticlesByIds :: forall r. Array String -> Run (AFF + READER Environment + r) (Array ArticleJson)
runFindArticlesByIds ids = do
  env <- ask
  runDriver env.host $ findArticlesByIds ids

runFindArticleById :: forall r. String -> Run (AFF + READER Environment + r) (Maybe ArticleJson)
runFindArticleById id = do
  env <- ask
  runDriver env.host $ findArticleById id

runFindArtilesByTitle :: forall r. String -> Run (AFF + READER Environment + r) (Array ArticleJson)
runFindArtilesByTitle title = do 
  env <- ask
  runDriverWithMock env.host $ findArticlesByTitle title
--------------------------------------------------------------------------------------------

type ArticleDriverType = {
  findArticleIdsByTitle :: forall r. String -> Run (AFF + READER Environment + r) (Array ArticleIndexJson),
  findArticleById :: forall r. String -> Run (AFF + READER Environment + r) (Maybe ArticleJson)
}

makeArticleDriver :: ArticleDriverType
makeArticleDriver = {
  findArticleIdsByTitle: \title -> runFindArticleIndicesByTitle title,
  findArticleById: \id -> runFindArticleById id
}

makeArticleDriverMock :: ArticleDriverType
makeArticleDriverMock = {
  findArticleIdsByTitle: \title -> do
    env <- ask
    runDriverWithMock env.host $ findArticleIndicesByTitle title,
  findArticleById: \id -> do
    env <- ask
    runDriverWithMock env.host $ findArticleById id
}

main :: Effect Unit
main = launchAff_ do
  response <- runFindArtilesByTitle "TestTitle"             -- AFF + READERのRunが返る
              # flip runReader {host: "http://article-api"} -- READERのエフェクトを除去 flipしているのはrunReaderの第一引数がrunだが渡ってくるのは第二引数だから
              # runBaseAff                                  -- AFFを除去
  liftEffect $ log $ show $ length response
  liftEffect $ log $ intercalate ", " $ (\a -> "title=" <> a.title <> ", body=" <> a.body) <$> response
