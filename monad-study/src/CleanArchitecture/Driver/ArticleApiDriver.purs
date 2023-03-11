module CleanArchitecture.Driver.ArticleApiDriver where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Control.Monad.Rec.Class (Step(..))
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, intercalate, length)
import Data.Either (Either(..))
import Data.Functor.Variant (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Simple.JSON (readJSON)
import Study.Control.Monad.Run.Run (AFF, Run, extract, interpret, lift, liftAff, runAccumPure, runBaseAff, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

{-
  接続情報が必要。何らかの手段で取得できる必要がある。
  driverを作る？ときに既に設定もinjectできてしまって(つまり関数の状態で、その場では実行されないようにしておいて)おくのがよいか？
  ということはRunになってるのが都合がよさそうだ。
-}

type ArticleIndexJson = {id :: String}
type ArticleJson = {title :: String, body :: String, author :: String}

data ArticleDriver a = 
   FindArticleIndicesByTitle String (Array ArticleIndexJson -> a)
 | FindArticlesByIds (Array String) (Array ArticleJson -> a)
 | FindArticleById String (Maybe ArticleJson -> a)

derive instance functorArticleDriver :: Functor ArticleDriver
_articleDriver = Proxy :: Proxy ("articleDriver")
type ARTICLE_DRIVER r = (articleDriver :: ArticleDriver | r)

-- このあたりで接続情報がほしい
-- どうするか

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

handleArticleDriver :: forall a. ArticleDriver a -> a
handleArticleDriver = case _ of
  (FindArticleIndicesByTitle title next) -> next [{id: ""}]
  (FindArticlesByIds ids next) -> next $ [{title: "", body: "", author: ""}]
  (FindArticleById id next) -> next $ Just {title: "", body: "", author: ""}

-- こいつらをどうくっつけたものか

_findArticleIndicesByTitle :: String -> Aff (Array ArticleIndexJson)
_findArticleIndicesByTitle title = do
  res <- AX.get ResponseFormat.string $ "http://article-api/articles?title" <> title
  case res of
    Left err -> do
      --pure $ Left $ "GET /api response failed to decode: " <> printError err
      pure []
    Right response -> do
      case readJSON response.body of
        Left e -> pure [] -- pure $ Left $ show e -- do log $ "Can't parse JSON. " <> show e
        Right (r :: Array ArticleIndexJson) -> pure r -- pure $ Right r -- do log $ "article id is: " <> show r.id

_findArticlesByIds ∷ Array String → Aff (Array ArticleJson)
_findArticlesByIds ids = do
  let
    urls = (\id -> "http://article-api/articles/" <> id) <$> ids
  responses <- parTraverse _findArticleById urls
  pure (catMaybes responses)

_findArticleById :: String -> Aff (Maybe ArticleJson)
_findArticleById id = do
  res <- AX.get ResponseFormat.string $ "http://article-api/articles/" <> id
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
handleDriver :: forall r. ArticleDriver ~> Run (AFF + r)
handleDriver = case _ of
  FindArticleIndicesByTitle title next -> do
    indices <- liftAff $ _findArticleIndicesByTitle title
    pure $ next indices
  FindArticlesByIds ids next -> do
    article <- liftAff $ _findArticlesByIds ids
    pure $ next article
  FindArticleById id next -> do
    article <- liftAff $ _findArticleById id
    pure $ next article

-- 固定値を返すhandler(mock)
-- handlerをこいつで上書きできればテストで差し替えられる
handleDriverMock :: forall r. ArticleDriver ~> Run (AFF + r)
handleDriverMock = case _ of
  FindArticleIndicesByTitle title next -> pure $ next [{id: "idBy" <> title}]
  FindArticlesByIds ids next -> pure $ next $ [{title: "title", body: intercalate "," ids, author: "author"}]
  FindArticleById _ next -> pure $ next $ Just {title: "", body: "", author: ""}

-------------------------------------------------------------------------------------------
{-
  Driverのエフェクトを除去したRunを返す
  (Driverの処理を実行でき、かつ型としてARTICLE_DRIVERが取り除かれた AFF + rのRunを返す)
-}
runDriver :: forall r. Run (AFF + ARTICLE_DRIVER + r) ~> Run (AFF + r)
runDriver = interpret (on _articleDriver handleDriver send)

-- mock用
runDriverWithMock :: forall r. Run (AFF + ARTICLE_DRIVER + r) ~> Run (AFF + r)
runDriverWithMock = interpret (on _articleDriver handleDriverMock send)

-------------------------------------------------------------------------------------------
{-
  色々な処理を実行するRunを返すやつら
  基本的に runDriver に処理対象の Run を渡しているだけ。
-}
runFindArticleIndicesByTitle :: forall r. String -> Run (AFF + r) (Array ArticleIndexJson)
-- runFindArticleIndicesByTitle title = findArticlesByTitle title # runDriver
runFindArticleIndicesByTitle title = runDriver $ findArticleIndicesByTitle title

runFindArticlesByIds :: forall r. Array String -> Run (AFF + r) (Array ArticleJson)
-- runFindArticlesByIds ids = findArticlesByIds ids # runDriver
runFindArticlesByIds ids = runDriver $ findArticlesByIds ids

runFindArticleById :: forall r. String -> Run (AFF + r) (Maybe ArticleJson)
--runFindArticleById id = findArticleById id # runDriver
runFindArticleById id = runDriver $ findArticleById id

runFindArtilesByTitle :: forall r. String -> Run (AFF + r) (Array ArticleJson)
runFindArtilesByTitle title = runDriverWithMock $ findArticlesByTitle title
--------------------------------------------------------------------------------------------

main :: Effect Unit
main = launchAff_ do
  response <- runBaseAff $ runFindArtilesByTitle "TestTitle"
  liftEffect $ log $ show $ length response
  liftEffect $ log $ intercalate ", " $ (\a -> "title=" <> a.title <> ", body=" <> a.body) <$> response
