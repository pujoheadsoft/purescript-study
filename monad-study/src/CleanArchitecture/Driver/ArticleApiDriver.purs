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
import Study.Control.Monad.Run.Run (AFF, Run, interpret, lift, liftAff, runBaseAff, send)
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

derive instance functorArticleDriver :: Functor ArticleDriver
_articleDriver = Proxy :: Proxy ("articleDriver")
type ARTICLE_DRIVER r = (articleDriver :: ArticleDriver | r)

-- このあたりで接続情報がほしい
-- どうするか

findArticlesByTitle :: forall r. String -> Run (ARTICLE_DRIVER + r) (Array ArticleIndexJson)
findArticlesByTitle title = lift _articleDriver $ FindArticlesByTitle title identity

findArticlesByIds :: forall r. Array String -> Run (ARTICLE_DRIVER + r) (Array ArticleJson)
findArticlesByIds ids = lift _articleDriver $ FindArticlesByIds ids identity

handleArticleDriver :: forall a. ArticleDriver a -> a
handleArticleDriver = case _ of
  (FindArticlesByTitle title next) -> next [{id: ""}]
  (FindArticlesByIds ids next) -> next $ [{title: "", body: "", author: ""}]

-- こいつらをどうくっつけたものか

xxx :: String -> Aff (Array ArticleIndexJson)
xxx title = do
  res <- AX.get ResponseFormat.string $ "http://article-api/articles?title" <> title
  case res of
    Left err -> do
      --pure $ Left $ "GET /api response failed to decode: " <> printError err
      pure []
    Right response -> do
      case readJSON response.body of
        Left e -> pure [] -- pure $ Left $ show e -- do log $ "Can't parse JSON. " <> show e
        Right (r :: Array ArticleIndexJson) -> pure r -- pure $ Right r -- do log $ "article id is: " <> show r.id

yyy ∷ Array String → Aff (Array ArticleJson)
yyy ids = do
  let
    urls = (\id -> "http://article-api/articles/" <> id) <$> ids
  responses <- parTraverse zzz urls
  pure (catMaybes responses)

zzz :: String -> Aff (Maybe ArticleJson)
zzz id = do
  res <- AX.get ResponseFormat.string $ "http://article-api/articles/" <> id
  case res of
    Left err -> pure Nothing
    Right response -> do
      case readJSON response.body of
        Left e -> pure Nothing
        Right (r :: ArticleJson) -> pure $ Just r

handleDriver :: forall r. ArticleDriver ~> Run (AFF + r)
handleDriver = case _ of
  FindArticlesByTitle title next -> do
    articles <- liftAff $ xxx title
    pure $ next articles
  FindArticlesByIds ids next -> do
    article <- liftAff $ yyy ids
    pure $ next article

-- 固定値を返すmock
-- handlerをこいつで上書きできればテストで差し替えられる
handleDriverMock :: forall r. ArticleDriver ~> Run (AFF + r)
handleDriverMock = case _ of
  FindArticlesByTitle _ next -> pure $ next [{id: ""}]
  FindArticlesByIds _ next -> pure $ next $ [{title: "", body: "", author: ""}]

runDriver :: forall r. Run (AFF + ARTICLE_DRIVER + r) ~> Run (AFF + r)
runDriver = interpret (on _articleDriver handleDriver send)

-- ここいらはgatewayから呼ぶやつかなあ
program1 :: forall r. String -> Run (AFF + r) (Array ArticleIndexJson)
program1 title = findArticlesByTitle title # runDriver

program2 :: forall r. Array String -> Run (AFF + r) (Array ArticleJson)
program2 ids = findArticlesByIds ids # runDriver
       
program3 :: forall r. String -> Run (ARTICLE_DRIVER + r) (Array ArticleJson)
program3 title = do
  indexes <- findArticlesByTitle title
  articles <- findArticlesByIds $ (\index -> index.id) <$> indexes
  pure articles


main :: Aff (Array ArticleIndexJson)
main = runBaseAff $ program1 "title"


-- program2 :: Effect Unit
-- program2 = program # runCont go done
--   where
--   go = match
--     { log: \(Log str cb) -> Console.log str *> cb
--     , sleep: \(Sleep ms cb) -> void $ setTimeout ms cb
--     }

--   done _ = do
--     Console.log "Done!"