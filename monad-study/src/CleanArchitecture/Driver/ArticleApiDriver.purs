module CleanArchitecture.Driver.ArticleApiDriver where

import Prelude

{-
  接続情報が必要。何らかの手段で取得できる必要がある。
  driverを作る？ときに既に設定もinjectできてしまって(つまり関数の状態で、その場では実行されないようにしておいて)おくのがよいか？
  ということはRunになってるのが都合がよさそうだ。
-}

type ArticleIndexJson = {id :: String}
type ArticleJson = {title :: String, body :: String, author :: String}

data ArticleDriver a = 
   FindArticleByTitle String (Array ArticleIndexJson -> a)
 | FindArticleById String (ArticleJson -> a)

findArticleByTitle :: String -> Array ArticleIndexJson
findArticleByTitle title = [{id: "id1"}]
