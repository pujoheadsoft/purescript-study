module Port.ArticlePort where

import Prelude

import Domain.Article (Article)
import Driver.ArticleDriver (class ArticleDriver, findJsonById)
import Driver.ArticleESDriver (class ArticleESDriver, findIndexByTitle)
import Run (Run)
import Run.Reader (READER)
import Type.Row (type (+))

class Monad m <= ArticlePort m where
  findByTitle :: String -> m Article

{-
  Tagless Final を利用するアプローチ
  本来はArticleGatewayにあるべきだが、ArticleGatewayの方にコメントしたようにインスタンスはここに定義しないとコンパイルエラーになる。
  しかしここに定義してしまうと、PortにDriverが漏れてしまうのでNG
-}
instance instancePort :: (ArticleESDriver m, ArticleDriver m) => ArticlePort m where
  findByTitle :: String -> m Article
  findByTitle title = do
    index <- findIndexByTitle title
    json <- findJsonById index.id
    pure {title: json.title}

{-
  これもTagless Finalを利用するアプローチ
  ↑と同様にDriverが漏れてしまっているのでNG
-}
type ArticlePortTypeBadDependency = {
  findByTitle :: forall m. ArticleESDriver m => ArticleDriver m => String -> m Article
}

{-
  Driverが出てこないパターン
-}
-- パラメーター `m` を受け取れるようにしている理由は、こうすることによりこのTypeが扱うモナドを明示できるようになり、
-- Mockを使ったテストが可能になるため。
type ArticlePortType m = {
  findByTitle :: String -> m Article
}

type ArticleRunPortType = {
  find :: forall r. Run (READER String + r) Article
}


