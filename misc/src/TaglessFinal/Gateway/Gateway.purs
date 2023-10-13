module TaglessFinal.Gateway.Gateway where

import Prelude

import TaglessFinal.Driver.Driver (findIndexByTitle, findJsonById)
import TaglessFinal.Port.Port (ArticlePortFunction, ArticlePresenterFunction)
import Type.Row (type (+))

createArticlePortFunction :: Record (ArticlePortFunction + ())
createArticlePortFunction = {
  findByTitle: \title -> do
    index <- findIndexByTitle title
    json <- findJsonById index.id
    pure {title: json.title}
}
