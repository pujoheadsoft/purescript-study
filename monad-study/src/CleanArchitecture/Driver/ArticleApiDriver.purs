module CleanArchitecture.Driver.ArticleApiDriver where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Control.Parallel (parTraverse)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Functor.Variant (on)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Simple.JSON (readJSON)
import Study.Control.Monad.Run.Run (AFF, Run, extract, interpret, lift, liftAff, runBaseAff, send)
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
   FindArticlesByTitle String (Array ArticleIndexJson -> a)
 | FindArticlesByIds (Array String) (Array ArticleJson -> a)
 | FindArticleById String (Maybe ArticleJson -> a)

derive instance functorArticleDriver :: Functor ArticleDriver
_articleDriver = Proxy :: Proxy ("articleDriver")
type ARTICLE_DRIVER r = (articleDriver :: ArticleDriver | r)

-- このあたりで接続情報がほしい
-- どうするか

findArticlesByTitle :: forall r. String -> Run (ARTICLE_DRIVER + r) (Array ArticleIndexJson)
findArticlesByTitle title = lift _articleDriver $ FindArticlesByTitle title identity

findArticlesByIds :: forall r. Array String -> Run (ARTICLE_DRIVER + r) (Array ArticleJson)
findArticlesByIds ids = lift _articleDriver $ FindArticlesByIds ids identity

findArticleById :: forall r. String -> Run (ARTICLE_DRIVER + r) (Maybe ArticleJson)
findArticleById id = lift _articleDriver $ FindArticleById id identity

handleArticleDriver :: forall a. ArticleDriver a -> a
handleArticleDriver = case _ of
  (FindArticlesByTitle title next) -> next [{id: ""}]
  (FindArticlesByIds ids next) -> next $ [{title: "", body: "", author: ""}]
  (FindArticleById id next) -> next $ Just {title: "", body: "", author: ""}

-- こいつらをどうくっつけたものか

_findArticlesByTitle :: String -> Aff (Array ArticleIndexJson)
_findArticlesByTitle title = do
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

-- 実値を返すhandler
handleDriver :: forall r. ArticleDriver ~> Run (AFF + r)
handleDriver = case _ of
  FindArticlesByTitle title next -> do
    articles <- liftAff $ _findArticlesByTitle title
    pure $ next articles
  FindArticlesByIds ids next -> do
    article <- liftAff $ _findArticlesByIds ids
    pure $ next article
  FindArticleById id next -> do
    article <- liftAff $ _findArticleById id
    pure $ next article

-- 固定値を返すmock
-- handlerをこいつで上書きできればテストで差し替えられる
handleDriverMock :: forall r. ArticleDriver ~> Run (AFF + r)
handleDriverMock = case _ of
  FindArticlesByTitle _ next -> pure $ next [{id: ""}]
  FindArticlesByIds _ next -> pure $ next $ [{title: "", body: "", author: ""}]
  FindArticleById _ next -> pure $ next $ Just {title: "", body: "", author: ""}

runDriver :: forall r. Run (AFF + ARTICLE_DRIVER + r) ~> Run (AFF + r)
runDriver = interpret (on _articleDriver handleDriver send)

-- ここいらはgatewayから呼ぶやつかなあ
runFindArticlesByTitle :: forall r. String -> Run (AFF + r) (Array ArticleIndexJson)
runFindArticlesByTitle title = findArticlesByTitle title # runDriver

runFindArticlesByIds :: forall r. Array String -> Run (AFF + r) (Array ArticleJson)
runFindArticlesByIds ids = findArticlesByIds ids # runDriver

runFindArticleById :: forall r. String -> Run (AFF + r) (Maybe ArticleJson)
runFindArticleById id = findArticleById id # runDriver
       
program1 :: forall r. String -> Run (ARTICLE_DRIVER + r) (Array ArticleJson)
program1 title = do
  indexes <- findArticlesByTitle title
  articles <- findArticlesByIds $ (\index -> index.id) <$> indexes
  pure articles


main :: Aff (Array ArticleIndexJson)
main = runBaseAff $ runFindArticlesByTitle "title"

-- runFindArticlesByIds :: Effect Unit
-- runFindArticlesByIds = program # runCont go done
--   where
--   go = match
--     { log: \(Log str cb) -> Console.log str *> cb
--     , sleep: \(Sleep ms cb) -> void $ setTimeout ms cb
--     }

--   done _ = do
--     Console.log "Done!"