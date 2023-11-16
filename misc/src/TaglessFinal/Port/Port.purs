module TaglessFinal.Port.Port where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (class MonadState)
import Effect.Aff.Class (class MonadAff)
import TaglessFinal.Domain.Article (Article)
import TaglessFinal.State.State (State)
import Type.Equality (class TypeEquals, to)
import Type.Row (type (+))

class Monad m <= ArticlePort m where
  findByTitle :: String -> m Article

type ArticlePortFunction m r = (
  findByTitle :: String -> m Article
  | r
)

instance instancePortReaderT
  :: (MonadAff m, TypeEquals f (Record (ArticlePortFunction m + r)))
  => ArticlePort (ReaderT f m) where
  findByTitle title = ReaderT $ to >>> \f ->
    f.findByTitle title

class Monad m <= ArticlePresenterPort m where
  update :: String -> m Unit

type ArticlePresenterFunction m r = (
  update :: String -> m Unit
  | r
)

instance instanceArticlePresenterReaderT
  :: (MonadState State m 
   , TypeEquals f (Record (ArticlePresenterFunction m + r)))
  => ArticlePresenterPort (ReaderT f m) where
  update title = ReaderT $ to >>> \f ->
    f.update title
