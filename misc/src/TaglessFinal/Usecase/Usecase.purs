module TaglessFinal.Usecase.Usecase where

import Prelude

import Control.Monad.State (class MonadState)
import TaglessFinal.Port.Port (class ArticlePort, class ArticlePresenterPort, findByTitle, update)
import TaglessFinal.State.State (State)

execute 
  :: forall m
   . ArticlePort m
  => ArticlePresenterPort m
  => String
  -> m Unit
execute title = do
  article <- findByTitle title
  update article.title
