module Component.Router where

import Prelude

import AppM (AppM(..))
import Component.Utils (OpaqueSlot)
import Control.Monad.State.Class (class MonadState)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, snd)
import HState (STATE, modify2, modify3, runState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Run (Run(..), extract)
import Run.Reader (READER, ask, runReader)
import Type.Row (type (+))

type Article = {
  title :: String
}

type State
  = { article :: Article }

data Action
  = Initialize
  | Update

type ChildSlots
  = ( home :: OpaqueSlot Unit )


component :: forall q i m. H.Component q i Void m
component =
  H.mkComponent
    { initialState: \_ -> { article: {title: ""} }
    , render
    , eval: H.mkEval $ H.defaultEval { 
        initialize = Just Initialize,
        handleAction = _handleAction
      }
    }
  where
  render :: forall cs. State -> H.ComponentHTML Action cs m
  render { article } = 
    HH.div_
      [
        HH.button
          [ HE.onClick \_ -> Update ]
          [ HH.text "更新" ]
       ,HH.text article.title
      ]
  
  _handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  _handleAction = case _ of
    Initialize -> H.modify_ \state -> state { article = { title: "ニュース" } }
    --Update -> H.modify_ \state -> state { article = { title: "ニュース" } }
    Update -> xxxx
    
xxxx :: forall m. MonadState State m => m Unit
xxxx = do
  _update "新しいニュース"
    # runState { article: { title: "ニュース" } }
    # extract
    # snd

type ArticlePort = {
  find :: forall r. Run (READER String + r) Article
}

type ArticlePresenter = {
  update :: forall r m. MonadState State m => String -> Run ( STATE State + r ) (m Unit)
}

createPort :: ArticlePort
createPort = {
  find: _find
}

createPresenter :: ArticlePresenter
createPresenter = {
  update: _update
}

_find :: forall r. Run (READER String + r) Article
_find = do
  value <- ask
  pure {title: value}

_update :: forall r m. MonadState State m => String -> Run ( STATE State + r ) (m Unit)
_update title = do
  modify3 \state -> state { article = { title: title } }

usecase :: forall m r. MonadState State m => ArticlePort -> ArticlePresenter -> Run (READER String + STATE State + r) (m Unit)
usecase port presenter = do
  article <- port.find
  presenter.update article.title