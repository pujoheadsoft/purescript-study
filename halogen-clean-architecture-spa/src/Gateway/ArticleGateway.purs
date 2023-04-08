module Gateway.ArticleGateway where

import Prelude

import Domain.Article (Article)
import Driver.ArticleDriver (ArticleDriverType, findJsonById)
import Driver.ArticleESDriver (ArticleESDriverType, findIndexByTitle)
import Port.ArticlePort (ArticlePortType, ArticlePortTypeBadDependency)

{-
  Tagless Final にした Driver を利用するアプローチ (コンパイルエラーになるのでコメントアウト)
  Port のインスタンスを Port とは異なる module に実装しようとすると
  Orphan instance とみなされエラーになってしまうのでこのアプローチは使えない。
  Port が宣言されている module 内でインスタンスも宣言すればうまく動くが、それだと Port と Gateway を分ける意味がない。
-}
-- instance articleGateway :: (ArticleESDriver m, ArticleDriver m) => ArticlePort m where
--   findByTitle :: String -> m Article
--   findByTitle title = do
--     index <- findIndexByTitle title
--     json <- findJsonById index.id
--     pure {title: json.title}

{-
  typeを使うパターン
  ここだけを見るとうまくいってるように見えるが、Portの方の定義を見るとDriverが漏れてるので良くない
-}
createBadDependencyPort :: ArticlePortTypeBadDependency
createBadDependencyPort = {
  findByTitle: \title -> do
    index <- findIndexByTitle title
    json <- findJsonById index.id
    pure {title: json.title}
}

{-
  依存するDriverの存在をPortに漏らさないパターン
-}
createPort :: forall m. Monad m => ArticleESDriverType -> ArticleDriverType -> ArticlePortType m
createPort esDriver driver = {
  findByTitle: \title -> findByTitle title esDriver driver
}

findByTitle :: forall m. Monad m => String -> ArticleESDriverType -> ArticleDriverType -> m Article
findByTitle title esDriver driver =  do
  index <- esDriver.findIndexByTitle title
  json <- driver.findById index.id
  pure {title: json.title}