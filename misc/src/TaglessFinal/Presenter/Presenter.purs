module TaglessFinal.Presenter.Presenter where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.ReaderTEtaConversionTransformer (readerT)
import TaglessFinal.Domain.Article (Article)
import TaglessFinal.Port.Port (ArticlePresenterFunction)
import Type.Equality (class TypeEquals)

{-
  Presenter
  Gatewayと同じくここではじめてMonadStateとStateという具体的な型が登場する
-}
createArticlePresenterPortFunction :: forall m. Monad m => ArticleStatePortFunction m -> ArticlePresenterFunction m ()
createArticlePresenterPortFunction functions = {
  update: \articles -> runReaderT (update2 articles) functions
}

class Monad m <= ArticleStatePort m where
  update :: Array String -> m Unit

type ArticleStatePortFunction m = {
  update :: Array String -> m Unit
}

instance instancePortReaderT
  :: (Monad m, TypeEquals f (ArticleStatePortFunction m))
  => ArticleStatePort (ReaderT f m) where
  update = readerT _.update

update2 :: forall m. Monad m => ArticleStatePort m => Array Article -> m Unit
update2 articles = update []