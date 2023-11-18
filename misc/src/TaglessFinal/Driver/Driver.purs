module TaglessFinal.Driver.Driver where

import Prelude

import Control.Monad.State (class MonadState, modify_)
import Effect.Aff.Class (class MonadAff)
import TaglessFinal.Gateway.Gateway (ArticleData, ArticleDataRepositoryFunction, ArticleDataId)
import TaglessFinal.Presenter.Presenter (ArticleStatePortFunction)
import TaglessFinal.State.State (State)
import Type.Row (type (+))

findIndexByTitle :: forall m. MonadAff m => String -> m (Array ArticleDataId)
findIndexByTitle title = pure ["dummy"]

findJsonById :: forall m. MonadAff m => String -> m ArticleData
findJsonById id = pure {id: id, title: "test"}

createArticleDataRepositoryFunction:: forall m. MonadAff m => ArticleDataRepositoryFunction m
createArticleDataRepositoryFunction = {
  findIdsByTitle: findIndexByTitle,
  findById: findJsonById
}


update :: forall m. MonadState State m => Array String -> m Unit
update titles = modify_ \state -> state { titles = titles }

createArticleStatePortFunction:: forall m. MonadState State m => ArticleStatePortFunction m
createArticleStatePortFunction = {
  update
}