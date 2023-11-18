module TaglessFinal.Gateway.Gateway where

import Prelude

import Effect.Aff.Class (class MonadAff)
import TaglessFinal.Driver.Driver (findIndexByTitle, findJsonById)
import TaglessFinal.Port.Port (ArticlePortFunction)
import Type.Row (type (+))

{-
  Gateway
  Portに依存している。
  またこのレイヤーではじめてMonadAffという具体的なMonadが登場する。
-}
createArticlePortFunction :: forall m. MonadAff m => Record (ArticlePortFunction m + ())
createArticlePortFunction = {
  findByTitle: \title -> do
    index <- findIndexByTitle title
    json <- findJsonById ""
    pure [{title: json.title}]
}

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