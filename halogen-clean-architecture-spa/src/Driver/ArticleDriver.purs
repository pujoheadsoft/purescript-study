module Driver.ArticleDriver where

import Prelude

import Control.Monad.Reader (Reader, ask)
import Data.Maybe (Maybe(..))

type ArticleJson = {id :: String, title :: String}

class Monad m <= ArticleDriver m where
  findJsonById :: String -> m ArticleJson

instance driverImpl :: ArticleDriver Maybe where
  findJsonById id = Just {id: id, title: "test"}

instance driverImpl2 :: ArticleDriver (Reader String) where
  findJsonById id = do
    v <- ask
    pure {id: id, title: v}

type ArticleDriverType = {
  findById :: forall m. Monad m => String -> m ArticleJson
}

createArticleDriverType :: ArticleDriverType
createArticleDriverType = {
  findById: \id -> pure  {id: id, title: "test"}
}