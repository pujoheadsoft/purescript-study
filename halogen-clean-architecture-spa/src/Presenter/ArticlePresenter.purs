module Presenter.ArticlePresenter where

import Prelude

import Component.State (State)
import Control.Monad.State (class MonadState)
import Halogen as H
import Run (Run(..))

class Monad m <= ArticlePresenter m where
  upadte :: String -> m Unit

type ArticlePresenterType = {
  update :: forall m. MonadState State m => String -> m Unit
}

createArticlePresenterType :: ArticlePresenterType
createArticlePresenterType = {
  update: _update
}

_update :: forall m. MonadState State m => String -> m Unit
_update title = do
  H.modify_ \state -> state { article = { title: title } }


type ArticleRunPresenter = {
  update :: forall r m. MonadState State m => String -> Run (r) (m Unit)
}

createArticleRunPresenter :: ArticleRunPresenter
createArticleRunPresenter = {
  update: _update2
}

_update2 :: forall r m. MonadState State m => String -> Run (r) (m Unit)
_update2 title = do
  pure $ H.modify_ \state -> state { article = { title: title } }