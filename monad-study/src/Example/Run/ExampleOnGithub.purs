module Example.Run.ExampleOnGithub where

import Prelude

import Control.Monad.Rec.Class (Step(..))
import Data.Functor.Variant (on)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Study.Control.Monad.Run.Run (EFFECT, Run, interpret, lift, liftEffect, runAccumPure, runBaseEffect, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

{-
  purescript-runのgithubに載ってる例
  https://github.com/natefaubion/purescript-run/blob/master/test/Examples.purs
-}

-- Talk ----------------------------------------------------------------------
data Talk a              -- aは継続
  = Speak String a       -- 引数はString。これは何かを取得しないvoid的な関数。この場合はただ継続aを持っているだけ
  | Listen (String -> a) -- 引数はなしで、Stringを返す。何か値を取得する関数の場合は、取得する型を受け取り継続aを返す関数を持つ。
-- 構造としては、
--   型コンストラクタ 引数の型が0～n個 最後に (継続a OR (戻りの型 -> 継続a)
-- という形

derive instance functorTalk :: Functor Talk

type TALK r = (talk :: Talk | r)

_talk = Proxy :: Proxy "talk"

speak :: forall r. String -> Run (TALK + r) Unit
speak str = lift _talk (Speak str unit)

listen :: forall r. Run (TALK + r) String
listen = lift _talk (Listen identity)

-- このhandlerはRunへの自然変換関数
handleTalk :: forall r. Talk ~> Run (EFFECT + r)
handleTalk = case _ of
  Speak str next -> do
    liftEffect $ log str
    pure next
  Listen reply -> do
    pure (reply "I am Groot")

runTalk :: forall r. Run (EFFECT + TALK + r) ~> Run (EFFECT + r)
runTalk = interpret (on _talk handleTalk send)
-- 自然変換関数を使う場合は、interpretを使える。

------------------------------------------------------------------------------

-- Dinner --------------------------------------------------------------------
type IsThereMore = Boolean
type Bill = Int

data Food = Pizza | Cizburger

data Dinner a
  = Eat Food (IsThereMore -> a)
  | CheckPlease (Bill -> a)

derive instance functorDinner :: Functor Dinner

type DINNER r = (dinner :: Dinner | r)

_dinner = Proxy :: Proxy "dinner"

eat :: forall r. Food -> Run (DINNER + r) IsThereMore
eat food = lift _dinner (Eat food identity)

checkPlease :: forall r. Run (DINNER + r) Bill
checkPlease = lift _dinner (CheckPlease identity)

type Tally = { stock :: Int, bill :: Bill }

handleDinner :: forall a. Tally -> Dinner a -> Tuple Tally a
handleDinner tally = case _ of
  Eat _ reply
    | tally.stock > 0 ->
      let tally' = { stock: tally.stock - 1, bill: tally.bill + 1 }
      in Tuple tally' (reply true)
    | otherwise ->
      Tuple tally (reply false)
  CheckPlease reply ->
    Tuple tally (reply tally.bill)

runDinnerPure :: forall r a. Tally -> Run (DINNER + r) a -> Run r (Tuple Bill a)
runDinnerPure = runAccumPure
  (\tally -> on _dinner (Loop <<< handleDinner tally) Done)
  (\tally a -> Tuple tally.bill a)
------------------------------------------------------------------------------

-- TalkとDinnerを合成 ---------------------------------------------------------
type LovelyEvening r = (TALK + DINNER + r)

dinnerTime :: forall r. Run (LovelyEvening r) Unit
dinnerTime = do
  speak "I'm famished!"
  isThereMore <- eat Pizza
  if isThereMore then dinnerTime -- 再帰
  else do
    bill <- checkPlease
    speak $ "Outrageous! " <> show bill

program :: forall r. Run (EFFECT + DINNER + r) Unit
program = dinnerTime # runTalk

program2 :: forall r. Run (EFFECT + r) (Tuple Bill Unit)
program2 = program # runDinnerPure { stock: 10, bill: 0 }

main :: Effect (Tuple Bill Unit)
main = runBaseEffect program2