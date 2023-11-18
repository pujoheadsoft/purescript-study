module TaglessFinal.Gateway.Gateway where

import Prelude

import Effect.Aff.Class (class MonadAff)
import TaglessFinal.Driver.Driver (findIndexByTitle, findJsonById)
import TaglessFinal.Port.Port (ArticlePortFunction)
import Type.Row (type (+))

{-
  Gateway
  Portに依存している。
  またこのレイヤーではじめてMonadAffという具体的なMonadが登場する。
-}
createArticlePortFunction :: forall m. MonadAff m => Record (ArticlePortFunction m + ())
createArticlePortFunction = {
  findByTitle: \title -> do
    index <- findIndexByTitle title
    json <- findJsonById index.id
    pure {title: json.title}
}

