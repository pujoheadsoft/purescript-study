module CleanArchitecture.Driver.ArticleApiDriver where

import Prelude

import Study.Control.Monad.Run.Run (Run, lift)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

{-
  接続情報が必要。何らかの手段で取得できる必要がある。
  driverを作る？ときに既に設定もinjectできてしまって(つまり関数の状態で、その場では実行されないようにしておいて)おくのがよいか？
  ということはRunになってるのが都合がよさそうだ。
-}

type ArticleIndexJson = {id :: String}
type ArticleJson = {title :: String, body :: String, author :: String}

data ArticleDriver a = 
   FindArticlesByTitle String (Array ArticleIndexJson -> a)
 | FindArticleById String (ArticleJson -> a)

derive instance functorArticleDriver :: Functor ArticleDriver
_articleDriver = Proxy :: Proxy ("articleDriver")
type ARTICLE_DRIVER r = (articleDriver :: ArticleDriver | r)

findArticlesByTitle :: forall r. String -> Run (ARTICLE_DRIVER + r) (Array ArticleIndexJson)
findArticlesByTitle title = lift _articleDriver $ FindArticlesByTitle title identity

findArticleById :: forall r. String -> Run (ARTICLE_DRIVER + r) ArticleJson
findArticleById id = lift _articleDriver $ FindArticleById id identity

-- ここで接続情報がほしい
-- どうするか
handleArticleDriver :: forall a. ArticleDriver a -> a
handleArticleDriver = case _ of
  (FindArticlesByTitle title next) -> next [{id: ""}]
  (FindArticleById id next) -> next {title: "", body: "", author: ""}