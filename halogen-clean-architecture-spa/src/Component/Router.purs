module Component.Router where

import Prelude

import Component.State (State)
import Component.Utils (OpaqueSlot)
import Control.Monad.State.Class (class MonadState)
import Data.Maybe (Maybe(..))
import Domain.Article (Article)
import Driver.ArticleDriver (createArticleDriverType)
import Driver.ArticleESDriver (createArticleESDriverType)
import Gateway.ArticleGateway (createPort)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Port.ArticlePort (createArticleRunPort)
import Presenter.ArticlePresenter (createArticlePresenterType, createArticleRunPresenter)
import Run (Run, extract)
import Run.Reader (READER, ask, runReader)
import Type.Row (type (+))
import Usecase.FindArticle (findArticleByRun, findArticleByType)

data Action
  = Initialize
  | Update
  | Update2

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
          [ HH.text "更新" ],
        HH.button
          [ HE.onClick \_ -> Update2 ]
          [ HH.text "更新2" ]
       ,HH.text article.title
      ]
  
  _handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  _handleAction = case _ of
    Initialize -> H.modify_ \state -> state { article = { title: "ニュース" } }
    -- Runを利用するパターン
    Update -> findArticleByRun createArticleRunPort createArticleRunPresenter
              # runReader "新しいニュース" 
              # extract
    -- Runは利用しないパターン
    Update2 -> do
                let
                  port = createPort createArticleESDriverType createArticleDriverType
                findArticleByType port createArticlePresenterType

