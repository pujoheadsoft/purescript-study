module TaglessFinal.Driver.Driver where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (class MonadState, modify_)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import TaglessFinal.State.State (State)
import Type.Equality (class TypeEquals, to)
import Type.Row (type (+))

-- 型クラスにしなくてもいいかもしれねえ

type ArticleIndexJson = {id :: String}

class Monad m <= ArticleESDriver m where
  findIndexByTitle :: String -> m (Array ArticleIndexJson)

instance esDriverAff :: MonadAff m => ArticleESDriver m where
  findIndexByTitle title = pure [{id: "dummy"}]


type ArticleJson = {id :: String, title :: String}

class Monad m <= ArticleDriver m where
  findJsonById :: String -> m ArticleJson

instance articleDriverAff :: MonadAff m => ArticleDriver m where
  findJsonById id = pure {id: id, title: "test"}

class Monad m <= ArticleStateDriver m where
  update :: Array String -> m Unit

instance instanceArticleStateDriver :: MonadState State m => ArticleStateDriver m where
  --update :: forall m. MonadState State m => String -> m Unit
  update titles = modify_ \state -> state { titles = titles }
