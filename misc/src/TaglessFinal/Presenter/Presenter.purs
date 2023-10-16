module TaglessFinal.Presenter.Presenter where

import Prelude

import Control.Monad.State (class MonadState)
import TaglessFinal.Driver.Driver as Driver
import TaglessFinal.Port.Port (ArticlePresenterFunction)
import TaglessFinal.State.State (State)
import Type.Row (type (+))

createArticlePresenterPortFunction :: forall m. MonadState State m => Record (ArticlePresenterFunction m + ())
createArticlePresenterPortFunction = {
  update: \title -> Driver.update title
}
