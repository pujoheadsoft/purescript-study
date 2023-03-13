module CleanArchitecture.Gateway.ArticleGateway2 where

import Prelude

import CleanArchitecture.Domain.Article (Article, ArticleId, ArticleIds, ArticleTitle)
import CleanArchitecture.Driver.ArticleApiDriver2 (ArticleDriver)
import Data.Maybe (Maybe)
import Study.Control.Monad.Run.Except (EXCEPT)
import Study.Control.Monad.Run.Run (Run, AFF)
import Type.Row (type (+))

type ArticleGateway = {
  findArticleIdsByTitle ::  forall r. ArticleTitle -> Run (AFF + EXCEPT String + r) ArticleIds,
  findArticleById ∷ forall r. ArticleId -> Run (AFF + EXCEPT String + r) (Maybe Article)
}

createArticleGateway :: ArticleDriver -> ArticleGateway
createArticleGateway driver = {
  findArticleIdsByTitle: \title -> findArticleIdsByTitle2 title driver,
  findArticleById: \id -> findArticleById2 id driver
}

findArticleIdsByTitle2 :: forall r. ArticleTitle -> ArticleDriver-> Run (AFF + EXCEPT String + r) ArticleIds
findArticleIdsByTitle2 title driver = do
  indexJsons <- driver.findArticleIdsByTitle title
  pure $ (\i -> i.id) <$> indexJsons

findArticleById2 ∷ forall r. ArticleId -> ArticleDriver -> Run (AFF + EXCEPT String + r) (Maybe Article)
findArticleById2 id driver = do
  articleJson <- driver.findArticleById id
  pure $ (\a -> {id: id, title: a.title, body: a.body, author: a.author}) <$> articleJson
