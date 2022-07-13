module CleanArchitecture.Gateway.ArticleGateway where

import Prelude

import CleanArchitecture.Domain.Article (ArticleTitle, Articles)
import CleanArchitecture.Driver.ArticleApiDriver as Driver
import Data.Array (catMaybes)

findArticlesByTitle :: ArticleTitle -> Articles
findArticlesByTitle title = [{title: "", body: "", author: ""}]


-- findArticlesByTitle2 title = do
--   indexes <- Driver.findArticlesByTitle title
--   articles <- Driver.findArticleById ""
--   xxx <- catMaybes [articles]
--   (\article -> {title: article.title, body: article.body, author: article.author}) <$> xxx