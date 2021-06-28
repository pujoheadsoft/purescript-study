module Component.Router where

import Prelude

import Capability.Navigate (class Navigate, navigate)
import Component.Utils (OpaqueSlot)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Route (Route(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH

data Query a
  = Navigate Route a

type State
  = { route :: Maybe Route }

data Action
  = Initialize

type ChildSlots
  = ( home :: OpaqueSlot Unit )

-- component
--   :: forall m q.
--      Navigate m
--   => H.Component q Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> { route: Nothing }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { initialize = Just Initialize
              }
    }
  where
  -- handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  -- handleAction = case _ of
  --   Initialize -> do
  --     navigate Home
      
  -- render :: State -> H.ComponentHTML Action () m
  render { route } = case route of
    Just r -> case r of
      Home -> HH.div_ [ HH.text "home" ]
      EditArticle -> HH.div_ [ HH.text "home" ]
      ViewArticle -> HH.div_ [ HH.text "home" ]
    Nothing -> HH.div_ [ HH.text "That page wasn't found." ]
