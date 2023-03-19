module Usecase.FindArticle where

import Prelude

import Component.State (State)
import Control.Monad.State (class MonadState)
import Port.ArticlePort (class ArticlePort, ArticlePortType, ArticleRunPortType, findByTitle)
import Presenter.ArticlePresenter (class ArticlePresenter, ArticlePresenterType, ArticleRunPresenter, upadte)
import Run (Run)
import Run.Reader (READER)
import Type.Row (type (+))

{-
  Tagless Finalを利用するパターン
  ArtilePortとArticleGatewayを見ればわかるが、DriverがPortに漏れているので良くない
-}
findArticle :: forall m. ArticlePort m => ArticlePresenter m => String -> m Unit
findArticle title = do
  article <- findByTitle title
  upadte article.title

{-
  PortとPresenterのtypeを渡すパターン
  こちらはDriverが漏れていない
-}
findArticleByType :: forall m. MonadState State m => String -> ArticlePortType -> ArticlePresenterType -> m Unit
findArticleByType title port presenter = do
  article <- port.findByTitle title
  presenter.update article.title

findArticleByRun :: forall m r. MonadState State m => ArticleRunPortType -> ArticleRunPresenter -> Run (READER String + r) (m Unit)
findArticleByRun port presenter = do
  article <- port.find
  presenter.update article.title