module TaglessFinal.Gateway.Gateway where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Traversable (traverse)
import TaglessFinal.Domain.Article (Article)
import TaglessFinal.Port.Port (ArticlePortFunction)
import Type.Equality (class TypeEquals, to)

{-
  Gateway
  Portに依存している。
  またこのレイヤーではじめてMonadAffという具体的なMonadが登場する。
-}
createArticlePortFunction :: forall m. Monad m => ArticleDataRepositoryFunction m -> ArticlePortFunction m ()
createArticlePortFunction functions = {
  findByTitle: \title -> runReaderT (findByTitle title) functions
}

type ArticleDataId = String
type ArticleData = {id :: String, title :: String}

class Monad m <= ArticleDataRepository m where
  findIdsByTitle :: String -> m (Array ArticleDataId)
  findById :: String -> m ArticleData

type ArticleDataRepositoryFunction m = {
  findIdsByTitle :: String -> m (Array ArticleDataId),
  findById :: String -> m ArticleData
}

instance instancePortReaderT
  :: (Monad m, TypeEquals f (ArticleDataRepositoryFunction m))
  => ArticleDataRepository (ReaderT f m) where
  findIdsByTitle title = ReaderT $ to >>> \f -> f.findIdsByTitle title
  findById id = ReaderT $ to >>> \f -> f.findById id

findByTitle :: forall m. Monad m => ArticleDataRepository m => String -> m (Array Article)
findByTitle title = do
  ids <- findIdsByTitle title
  articles <- traverse findById ids
  pure $ (\a -> {title: a.title}) <$> articles

{-
  MonadAff を ArticlePortのインスタンスにしようとすると Orphan instance でコンパイルエラーになってしまう
  更にnewtypeでMonadAffのラッパーは作れない。

  なぜならばnewtypeは新しい型を定義するために使われるが
  MonadAff mが具体的な型ではなく制約を表す型クラスであるため。
-}
-- instance instancePort :: MonadAff m => ArticlePort m where
--   findByTitle title = do
--     index <- findIndexByTitle title
--     json <- findJsonById index.id
--     pure {title: json.title}

-- これはできない
-- newtype MonadAffWrapper = MonadAffWrapper (MonadAff m)