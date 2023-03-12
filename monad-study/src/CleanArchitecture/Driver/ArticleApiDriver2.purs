module CleanArchitecture.Driver.ArticleApiDriver2 where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import CleanArchitecture.Driver.ArticleJson (ArticleIndexJson, ArticleJson)
import CleanArchitecture.Driver.Environment (Environment)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Simple.JSON (readJSON)
import Study.Control.Monad.Run.Except (EXCEPT)
import Study.Control.Monad.Run.Run (AFF, Run, liftAff)
import Type.Row (type (+))

{-
  DriverというRunの型を作らなかった版
  独自のDSLを作りたいのでなければこちらで十分かと思われる
  あと例外処理できるようにした
  あと環境変数をReaderから読まないようにした
-}

_findArticleIndicesByTitle :: forall r. String -> Environment -> Run (AFF + EXCEPT String + r) (Array ArticleIndexJson)
_findArticleIndicesByTitle title env = liftAff go
  where 
  go = do
    res <- AX.get ResponseFormat.string $ env.host <> "/articles?title" <> title
    case res of
      Left err -> do
        --pure $ Left $ "GET /api response failed to decode: " <> printError err
        pure []
      Right response -> do
        case readJSON response.body of
          Left e -> pure [] -- pure $ Left $ show e -- do log $ "Can't parse JSON. " <> show e
          Right (r :: Array ArticleIndexJson) -> pure r -- pure $ Right r -- do log $ "article id is: " <> show r.id

_findArticleById :: forall r. String -> Environment -> Run (AFF + EXCEPT String + r) (Maybe ArticleJson)
_findArticleById id env = liftAff go
  where 
  go = do
    res <- AX.get ResponseFormat.string $ env.host <> "/articles/" <> id
    case res of
      Left err -> pure Nothing
      Right response -> do
        case readJSON response.body of
          Left e -> pure Nothing
          Right (r :: ArticleJson) -> pure $ Just r

type ArticleDriverType = {
  findArticleIdsByTitle :: forall r. String -> Run (AFF + EXCEPT String r) (Array ArticleIndexJson),
  findArticleById :: forall r. String -> Run (AFF + EXCEPT String + r) (Maybe ArticleJson)
}

makeArticleDriver2 :: Environment -> ArticleDriverType
makeArticleDriver2 env = {
  findArticleIdsByTitle: \title -> _findArticleIndicesByTitle title env,
  findArticleById: \id -> _findArticleById id env
}

makeArticleDriverMock2 :: Environment -> ArticleDriverType
makeArticleDriverMock2 env = {
  findArticleIdsByTitle: \title -> do
    pure [
      {id: "[ID1] 一致した条件 title:" <> title <> ", host:" <> env.host},
      {id: "[ID2] 一致した条件 title:" <> title <> ", host:" <> env.host}
    ],
  findArticleById: \id -> do
    pure $ Just {title: "title by id: " <> id, body: "", author: ""}
}
