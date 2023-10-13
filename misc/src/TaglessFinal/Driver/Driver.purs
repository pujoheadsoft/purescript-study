module TaglessFinal.Driver.Driver where

import Prelude

import Control.Monad.State (class MonadState, modify_)
import Effect.Aff (Aff)
import TaglessFinal.State.State (State)

type ArticleIndexJson = {id :: String}

class Monad m <= ArticleESDriver m where
  findIndexByTitle :: String -> m ArticleIndexJson

instance esDriverAff :: ArticleESDriver Aff where
  findIndexByTitle title = pure {id: "dummy"}


type ArticleJson = {id :: String, title :: String}

class Monad m <= ArticleDriver m where
  findJsonById :: String -> m ArticleJson

instance articleDriverAff :: ArticleDriver Aff where
  findJsonById id = pure {id: id, title: "test"}


class Monad m <= ArticleStateDriver m where
  update :: String -> m Unit

instance stateDriver :: MonadState State m => ArticleStateDriver m where
  update title = modify_ \state -> state { article = { title: title } }