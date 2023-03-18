module Driver.ArticleESDriver where

import Prelude

import Control.Monad.Reader (Reader, ask)
import Data.Maybe (Maybe(..))

type ArticleIndexJson = {id :: String}

class Monad m <= ArticleESDriver m where
  findIndexByTitle :: String -> m ArticleIndexJson

instance driverImpl :: ArticleESDriver Maybe where
  findIndexByTitle title = Just {id: "dummy"}

instance driverImpl2 :: ArticleESDriver (Reader String) where
  findIndexByTitle title = do
    v <- ask
    pure {id: "dummy"}

type ArticleESDriverType = {
  findIndexByTitle :: forall m. Monad m => String -> m ArticleIndexJson
}

createArticleESDriverType :: ArticleESDriverType
createArticleESDriverType = {
  findIndexByTitle: \_ -> pure {id: "dummy"}
}