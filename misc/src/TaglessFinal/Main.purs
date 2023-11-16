module TaglessFinal.Main where

import Prelude

import Control.Monad.RWS (runRWST)
import Control.Monad.Reader (ReaderT, lift, runReader, runReaderT)
import Control.Monad.State (class MonadState, StateT, evalStateT, execStateT, modify_, runState, runStateT)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Record.Builder (build, merge, union)
import TaglessFinal.Domain.Article (Article)
import TaglessFinal.Driver.Driver (findIndexByTitle, findJsonById)
import TaglessFinal.Driver.Driver as Driver
import TaglessFinal.Gateway.Gateway (createArticlePortFunction)
import TaglessFinal.Port.Port (class ArticlePort, class ArticlePresenterPort, ArticlePortFunction, ArticlePresenterFunction)
import TaglessFinal.Presenter.Presenter (createArticlePresenterPortFunction)
import TaglessFinal.State.State (State)
import TaglessFinal.Usecase.Usecase (execute)
import Type.Row (type (+))

class Monad m <= A m where
  a :: m String

class Monad m <= B m where
  b :: String -> m Unit

instance aInstance :: MonadAff m => A m where
  a = liftAff do
    delay (Milliseconds 1000.0)
    pure "hoge"

instance bInstance :: MonadState { value :: String } m => B m where
  b v = modify_ \state -> state { value = v }

test :: forall m. A m => B m => m Unit
test = do
  result <- a
  b result

-- main :: Effect Unit
-- main = launchAff_ $ do
--   log "★★★★★★★★★"
--   -- 例: Aff モナドを使用して test 関数を実行
--   result <- execStateT test {value: ""} 
--   log result.value
--   pure unit
--   log "★★★★★★★★★"

exec :: forall r s m a. Monad m => ReaderT r (StateT s m) a -> r -> s -> m s
exec r functions state = runReaderT r functions
  # flip execStateT state

main :: Effect Unit
main = launchAff_ do
  log "★★★★★★★★★"
  let
    functions = build (merge createArticlePortFunction) createArticlePresenterPortFunction

    --findByTitle :: forall m. MonadAff m => String -> m Article
    -- findByTitle = \title -> do
    --     index <- findIndexByTitle title
    --     json <- findJsonById index.id
    --     pure {title: json.title}
    
    --update :: forall m. MonadState State m => String -> m Unit
    --update = \title -> Driver.update title

    --functions :: forall m. MonadAff m => MonadState State m => Record (ArticlePortFunction m + ArticlePresenterFunction m + ())
    --functions = { findByTitle, update }

  -- result <- runReaderT (execute "title") functions
  --     # flip execStateT {article: {title: ""}}
  
  result <- exec (execute "title") functions {article: {title: ""}}
  
  log result.article.title
  log "★★★★★★★★★"
  pure unit

