module TaglessFinal.Presenter.Presenter where

import Prelude

import Control.Monad.State (class MonadState)
import TaglessFinal.Driver.Driver as Driver
import TaglessFinal.Port.Port (ArticlePresenterFunction)
import Type.Row (type (+))

createArticlePresenterPortFunction :: forall m s. MonadState s m => Record (ArticlePresenterFunction m + ())
createArticlePresenterPortFunction = {
  update: \title -> Driver.update title
}
