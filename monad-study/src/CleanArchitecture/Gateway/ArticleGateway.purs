module CleanArchitecture.Gateway.ArticleGateway where

import Prelude

import CleanArchitecture.Domain.Article (Article, ArticleId, ArticleIds, ArticleTitle)
import CleanArchitecture.Driver.ArticleApiDriver (ArticleDriverType)
import CleanArchitecture.Driver.Environment (Environment)
import Data.Maybe (Maybe)
import Study.Control.Monad.Run.Reader (READER)
import Study.Control.Monad.Run.Run (Run, AFF)
import Type.Row (type (+))

-------------------------------------------------------------------------------------------------

type ArticleGatewayType = {
  findArticleIdsByTitle ::  forall r. ArticleTitle -> Run ( AFF + READER Environment + r) ArticleIds,
  findArticleById ∷ forall r. ArticleId -> Run ( AFF + READER Environment + r) (Maybe Article)
}

makeArticleGateway :: ArticleDriverType -> ArticleGatewayType
makeArticleGateway driver = {
  findArticleIdsByTitle: \title -> findArticleIdsByTitle title driver,
  findArticleById: \id -> findArticleById id driver
}

findArticleIdsByTitle :: forall r. ArticleTitle -> ArticleDriverType-> Run ( AFF + READER Environment + r) ArticleIds
findArticleIdsByTitle title driver = do
  indexJsons <- driver.findArticleIdsByTitle title
  pure $ (\i -> i.id) <$> indexJsons

findArticleById ∷ forall r. ArticleId -> ArticleDriverType -> Run ( AFF + READER Environment + r) (Maybe Article)
findArticleById id driver = do
  articleJson <- driver.findArticleById id
  pure $ (\a -> {id: id, title: a.title, body: a.body, author: a.author}) <$> articleJson
