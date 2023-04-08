module Presenter.ArticlePresenter where

import Prelude

import Component.State (State)
import Control.Monad.State (class MonadState)
import Halogen as H
import Run (Run)

class Monad m <= ArticlePresenter m where
  upadte :: String -> m Unit

-- パラメーター `m` を受け取れるようにしている理由は、こうすることによりこのTypeが扱うモナドを明示できるようになり、
-- Mockを使ったテストが可能になるため。どのモナドを使うかという制約は↓の生成関数が持つ。
-- Typeの定義自体は抽象的にしておいた方が都合がよい。
type ArticlePresenterType m = {
  update :: String -> m Unit
}

createArticlePresenterType :: forall m. MonadState State m => ArticlePresenterType m
createArticlePresenterType = {
  update: _update
}

_update :: forall m. MonadState State m => String -> m Unit
_update title = do
  H.modify_ \state -> state { article = { title: title } }


type ArticleRunPresenter m = {
  update :: forall r. String -> Run (r) (m Unit)
}

createArticleRunPresenter :: forall m. MonadState State m => ArticleRunPresenter m
createArticleRunPresenter = {
  update: _update2
}

_update2 :: forall r m. MonadState State m => String -> Run (r) (m Unit)
_update2 title = do
  pure $ H.modify_ \state -> state { article = { title: title } }