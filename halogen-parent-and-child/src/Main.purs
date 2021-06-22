module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query as HQ
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI parent unit body

-- Row型
type Slots = (button :: ButtonSlot Int)

data ParentAction = HandleButton ButtonOutput

type ParentState = { clicked :: Int }

parent :: forall query input output m. MonadEffect m => H.Component query input output m
parent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: input -> ParentState
  initialState _ = { clicked: 0 }

  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render { clicked } = do
    let clicks = show clicked
    HH.div_
      [
        -- 子コンポーネントとの通信はslotを使って行う
        -- slot Proxy, 識別子, 子コンポーネント, 子コンポーネントへの入力, 子コンポーネントからの出力メッセージの処理方法(outputを受け取ってactionを返す関数)
        -- HandleButtonはButtonOutputを受け取ってParentActionにする関数
         HH.slot _button 0 button { label: clicks <> " Enabled " } HandleButton
        ,HH.slot _button 1 button { label: clicks <> " Power " } HandleButton
        ,HH.slot _button 2 button { label: clicks <> " Switch " } HandleButton
      ]
  
  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output m Unit
  handleAction = case _ of
    -- 子コンポーネントからのAction(Clickしかない)
    HandleButton output -> case output of
      Clicked -> do
        H.modify_ \state -> state { clicked = state.clicked + 1 }
        -- Proxyと識別子を指定してクエリを投げる。クエリには命令を投げるクエリと、子からの情報を要求するクエリがある。
        -- tellは命令を投げるクエリ、requestは情報を要求するクエリ。requestAllはすべてに対して要求を行う。
        -- クエリは子コンポーネントのhandleQueryでさばくことになる。
        H.tell _button 0 (SetEnabled true) -- この場合は0番目のみenabledになる。
        on <- HQ.requestAll _button GetEnabled -- すべてのボタンのenabledを取得
        logShow on

type ButtonSlot = H.Slot ButtonQuery ButtonOutput

_button = Proxy :: Proxy "button"

data ButtonQuery a
  = GetEnabled (Boolean -> a)
  | SetEnabled Boolean a

data ButtonOutput = Clicked

data ButtonAction = Click | Receive ButtonInput

type ButtonInput = { label :: String }

type ButtonState = { label :: String, enabled :: Boolean }

--
-- ButtonInputはevalのreceiveに指定するActionが受け取る入力の型
button :: forall m. H.Component ButtonQuery ButtonInput ButtonOutput m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery -- handleQueryには親からの要求をさばく関数を指定する
      , receive = Just <<< Receive -- receiveは親からの入力を処理するActionを指定(この場合Receive)する。
      }
    }
  where
  initialState :: ButtonInput -> ButtonState
  initialState { label } = { label, enabled: false }

  render :: ButtonState -> H.ComponentHTML ButtonAction () m
  render { label, enabled } =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text $ label <> " (" <> (if enabled then "on" else "off") <> ")"]
  
  handleAction :: ButtonAction -> H.HalogenM ButtonState ButtonAction () ButtonOutput m Unit
  handleAction = case _ of
    -- 受け取ったButtonInputを処理
    Receive input ->
      H.modify_ _ { label = input.label }
    
    -- raiseは出力。Outputの型である必要がある。ここで出力したものを親のhandleActionでさばける。
    Click -> do
      H.modify_ \state -> state { enabled = not state.enabled }
      H.raise Clicked
  
  -- 親からの要求にこたえる
  handleQuery :: forall a. ButtonQuery a -> H.HalogenM ButtonState ButtonAction () ButtonOutput m (Maybe a)
  handleQuery = case _ of
    SetEnabled value next -> do
      H.modify_ _ { enabled = value }
      pure (Just next)
    
    GetEnabled reply -> do
      enabled <- H.gets _.enabled
      pure (Just (reply enabled))