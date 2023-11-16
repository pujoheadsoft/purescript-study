module TaglessFinal.Gateway.Gateway where

import Prelude

import Effect.Aff.Class (class MonadAff)
import TaglessFinal.Driver.Driver (findIndexByTitle, findJsonById)
import TaglessFinal.Port.Port (ArticlePortFunction)
import Type.Row (type (+))

createArticlePortFunction :: forall m. MonadAff m => Record (ArticlePortFunction m + ())
createArticlePortFunction = {
  findByTitle: \title -> do
    index <- findIndexByTitle title
    json <- findJsonById index.id
    pure {title: json.title}
}

