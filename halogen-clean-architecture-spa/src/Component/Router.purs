module Component.Router where

import Prelude

import Component.Utils (OpaqueSlot)
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Halogen as H
import Halogen.HTML as HH

type State =
  {route :: Maybe Route}

data Action
  = Initialize

type ChildSlots = (home :: OpaqueSlot Unit)

component = H.mkComponent
  {initialState: \_ -> {route: Nothing}
  , render
  , eval: H.mkEval $ H.defaultEval 
  }
  where
  -- render :: State -> H.ComponentHTML Action () m
  render {route} = case route of
    Just r -> case r of
      Home ->
        HH.div_ [HH.text "home"]
      EditArticle ->
        HH.div_ [HH.text "home"]
      ViewArticle ->
        HH.div_ [HH.text "home"]
    Nothing ->
      HH.div_ [HH.text "That page wasn't found."]
