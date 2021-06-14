module Main where

import Data.Either
import Data.Maybe
import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), examplePerson, updateCity, updateFirstName, updateLastName, updatePhoneNumber, updateState, updateStreet)
import Data.AddressBook.Validation (Errors, validatePerson')
import Data.Array ((..), length, modifyAt, zipWith)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, logShow)
import Halogen (ClassName(..), Component, ComponentHTML, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen.Aff as HA
import Halogen.HTML (HTML, div, form, h3_, input, label, li_, text, ul_)
import Halogen.HTML.Events (onChange)
import Halogen.HTML.Properties (InputType(..), class_, placeholder, type_, value)
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event, target)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLInputElement (fromEventTarget, value, HTMLInputElement) as Input

type State
  = { person :: Person
    , errors :: Errors
    }

data Action
  = UpdateFirstName Event
  | UpdateLastName Event
  | UpdateStreet Event
  | UpdateCity Event
  | UpdateState Event
  | UpdatePhoneNumber Event Int

initialState :: forall input. input -> State
initialState _ =
  { person: examplePerson
  , errors: []
  }

-- HTML widgets action
formField :: forall widgets. String -> String -> String -> (Event -> Action) -> HTML widgets Action
formField name hint v a =
  div
    [ class_ (ClassName "form-group") ]
    [ label
        [ class_ (ClassName "col-sm-2 control-label") ]
        [ text name ]
    , div
        [ class_ (ClassName "col-sm-3") ]
        [ input
            [ type_ InputText
            , class_ (ClassName "form-control")
            , placeholder hint
            , value v
            , onChange \e -> a e
            ]
        ]
    ]

renderValidationError :: forall widgets action. String -> HTML widgets action
renderValidationError err = li_ [ text err ]

renderValidationErrors :: forall widgets action. Errors -> Array (HTML widgets action)
renderValidationErrors [] = []

renderValidationErrors xs =
  [ div [ class_ (ClassName "alert alert-danger") ]
      [ ul_ (map renderValidationError xs) ]
  ]

renderPhoneNumber :: forall widgets. PhoneNumber -> Int -> HTML widgets Action
renderPhoneNumber (PhoneNumber phone) index = formField (show phone."type") "XXX-XXX-XXXX" phone.number \event -> UpdatePhoneNumber event index -- 結局ほしいのはevent受け取ってactionを返す関数なので

render :: forall m. MonadEffect m => State -> ComponentHTML Action () m
render state = do
  -- render {person: (Person {firstName: firstName, lastName: lastName})} = do というようにパターンマッチしても firstNameたちにはアクセスできる
  let
    -- 名前付きパターン Haskellのasパターンマッチと同じ
    -- 全体@パターンと書くことで、入れ子の部分を分解しつつ、名前を付けてある全体にアクセスできる
    -- この場合Personにマッチしたものを personという名前で束縛している
    (Person person@{ homeAddress: Address address }) = state.person
  div
    [ class_ (ClassName "container") ]
    [ div
        [ class_ (ClassName "row") ]
        [ text (show person) ]
    , div
        [ class_ (ClassName "row") ]
        (renderValidationErrors state.errors)
    , div
        [ class_ (ClassName "row") ]
        [ form
            [ class_ (ClassName "form-horizontal") ]
            $ [ h3_
                  [ text "Basic Information" ]
              , formField "First Name" "First Name" person.firstName UpdateFirstName
              , formField "Last Name" "Last Name" person.lastName UpdateLastName
              , h3_ [ text "Address" ]
              , formField "Street" "Street" address.street UpdateStreet
              , formField "City" "City" address.city UpdateCity
              , formField "State" "State" address.state UpdateState
              , h3_ [ text "Contact Information" ]
              ]
            <> zipWith renderPhoneNumber person.phones (0 .. length person.phones)
        ]
    ]

validation :: State -> State
validation state@{ person } = case validatePerson' person of
  Left e -> state { errors = e }
  Right _ -> state { errors = [] }


handleAction :: forall output monad. MonadEffect monad => Action -> HalogenM State Action () output monad Unit
handleAction = case _ of
  UpdateFirstName event -> do
    v <- liftEffect (eventValue event)
    modify_ \state -> state { person = updateFirstName v state.person }
    modify_ validation
  UpdateLastName event -> do
    v <- liftEffect (eventValue event)
    modify_ \state -> state { person = updateLastName v state.person }
    modify_ validation
  UpdateStreet event -> do
    v <- liftEffect (eventValue event)
    modify_ \state -> state { person = updateStreet v state.person }
    modify_ validation
  UpdateCity event -> do
    v <- liftEffect (eventValue event)
    modify_ \state -> state { person = updateCity v state.person }
    modify_ validation
  UpdateState event -> do
    v <- liftEffect (eventValue event)
    modify_ \state -> state { person = updateState v state.person }
    modify_ validation
  UpdatePhoneNumber event index -> do
    v <- liftEffect (eventValue event)
    modify_ \state -> state { person = updatePhoneNumber index v state.person }
    modify_ validation

eventValue :: Event -> Effect String
eventValue event = targetValue (target event)

targetValue :: Maybe EventTarget -> Effect String
targetValue (Just et) = inputValue (Input.fromEventTarget et)

targetValue _ = pure ""

inputValue :: Maybe Input.HTMLInputElement -> Effect String
inputValue (Just el) = Input.value el

inputValue _ = pure ""

-- component :: forall query input output monad. Component query input output monad
component :: forall query input output monad. MonadEffect monad => Component query input output monad
component =
  mkComponent
    { initialState
    , render
    , eval: mkEval $ defaultEval { handleAction = handleAction }
    }

-- handleAction :: action -> HalogenM state action slots output monad Unit
-- action を受け取って HalogenM (Halogen component eval effect monad) を返す関数
main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
