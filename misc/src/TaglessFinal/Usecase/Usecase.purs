module TaglessFinal.Usecase.Usecase where

import Prelude

import TaglessFinal.Port.Port (class ArticlePort, class ArticlePresenterPort, findByTitle, update)

-- UseCaseは型クラスを組み合わせて使うだけ
-- Port(UseCaseのレイヤー)にしか依存しない
execute 
  :: forall m
   . ArticlePort m
  => ArticlePresenterPort m
  => String
  -> m Unit
execute title = do
  article <- findByTitle title
  update article.title
