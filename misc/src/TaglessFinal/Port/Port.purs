module TaglessFinal.Port.Port where

import Prelude

import TaglessFinal.Domain.Article (Article)
import Control.Monad.Reader (ReaderT(..))
import Effect.Aff (Aff)
import Type.Equality (class TypeEquals, to)
import Type.Row (type (+))

class Monad m <= ArticlePort m where
  findByTitle :: String -> m Article

type ArticlePortFunction r = (
  findByTitle :: String -> Aff Article
  | r
)

instance instancePortReaderT
  :: TypeEquals f (Record (ArticlePortFunction + r))
  => ArticlePort (ReaderT f Aff) where
  findByTitle title = ReaderT $ to >>> \f ->
    f.findByTitle title


class Monad m <= ArticlePresenterPort m where
  update :: String -> m Unit

type ArticlePresenterFunction m r = (
  update :: String -> m Unit
  | r
)

instance instanceArticlePresenterReaderT
  :: (Monad m 
   , TypeEquals f (Record (ArticlePresenterFunction m + r)))
  => ArticlePresenterPort (ReaderT f m) where
  update title = ReaderT $ to >>> \f ->
    f.update title
