module CleanArchitecture.Gateway.ArticleGateway where

import Prelude

import CleanArchitecture.Domain.Article (ArticleTitle, ArticleIds)
import CleanArchitecture.Driver.ArticleApiDriver (Environment)
import CleanArchitecture.Driver.ArticleApiDriver as Driver
import Study.Control.Monad.Run.Reader (READER)
import Study.Control.Monad.Run.Run (Run, AFF)
import Type.Row (type (+))

findArticlesByTitle :: forall r. ArticleTitle -> Run ( AFF + READER Environment + r) ArticleIds
findArticlesByTitle title = do
  indices <- Driver.runFindArticleIndicesByTitle title
  pure $ (\i -> i.id) <$> indices
--  xxx <- catMaybes [articles]
--  (\article -> {title: article.title, body: article.body, author: article.author}) <$> xxx