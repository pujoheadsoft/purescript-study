module Component.Router where

import Prelude

import Component.Utils (OpaqueSlot)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Article = {
  title :: String
}

type State
  = { article :: Article }

data Action
  = Initialize
  | Search

type ChildSlots
  = ( home :: OpaqueSlot Unit )


component :: forall q i m. H.Component q i Void m
component =
  H.mkComponent
    { initialState: \_ -> { article: {title: ""} }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { initialize = Just Initialize,
              handleAction = _handleAction
              }
    }
  where
  render :: forall cs. State -> H.ComponentHTML Action cs m
  render { article } = 
    HH.div_
      [
        HH.button
          [ HE.onClick \_ -> Search ]
          [ HH.text "Search" ]
       ,HH.text article.title
      ]

  _handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  _handleAction = case _ of
    Initialize -> H.modify_ \state -> state { article = { title: "ニュース" } }
    Search -> H.modify_ \state -> state { article = { title: "検索されたニュース" } }