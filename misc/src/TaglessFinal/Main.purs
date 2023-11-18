module TaglessFinal.Main where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (class MonadState, StateT, execStateT)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log, logShow)
import Record.Builder (build, merge)
import TaglessFinal.Domain.Article (Article)
import TaglessFinal.Driver.Driver (createArticleDataRepositoryFunction, createArticleStatePortFunction, findIndexByTitle, findJsonById)
import TaglessFinal.Driver.Driver as Driver
import TaglessFinal.Gateway.Gateway (createArticlePortFunction)
import TaglessFinal.Port.Port (ArticlePortFunction, ArticlePresenterFunction)
import TaglessFinal.Presenter.Presenter (createArticlePresenterPortFunction)
import TaglessFinal.State.State (State)
import TaglessFinal.Usecase.Usecase (execute)
import Type.Row (type (+))

-- helper関数
exec :: forall r s m a. Monad m => ReaderT r (StateT s m) a -> r -> s -> m s
exec r functions state = runReaderT r functions
  # flip execStateT state

main :: Effect Unit
main = launchAff_ do
  log "★★★★★★★★★"
  let
    -- 関数が定義されたレコードをマージ
    functions = build (merge (createArticlePortFunction createArticleDataRepositoryFunction)) 
      (createArticlePresenterPortFunction createArticleStatePortFunction)
  
  result <- exec (execute "title") functions {titles: []}
  
  logShow result
  log "★★★★★★★★★"
  pure unit

-- 冗長に型定義を書き、レコードのマージなどもしない版（参考までに用意）
-- main2 :: Effect Unit
-- main2 = launchAff_ do
--   log "★★★★★★★★★"
--   let
--     findByTitle :: forall m. MonadAff m => String -> m Article
--     findByTitle = \title -> do
--       index <- findIndexByTitle title
--       json <- findJsonById index.id
--       pure {title: json.title}
    
--     update :: forall m. MonadState State m => String -> m Unit
--     update = \title -> Driver.update title

--     functions :: forall m. MonadAff m => MonadState State m => Record (ArticlePortFunction m + ArticlePresenterFunction m + ())
--     functions = { findByTitle, update }

--   result <- runReaderT (execute "title") functions
--       # flip execStateT {article: {title: ""}}
    
--   log result.article.title
--   log "★★★★★★★★★"
--   pure unit

