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

{-
  speakもlistenもRunを返す
  具体的なロジックが書かれてるわけではなく、Talk型を作ってそれをもとにRun (TALK) を作っているだけ。
  処理は↓のhandleTalkに書かれている。
-}
speak :: forall r. String -> Run (TALK + r) Unit
speak str = lift _talk (Speak str unit)

listen :: forall r. Run (TALK + r) String
listen = lift _talk (Listen identity)

{-
  渡されたTalk型によって処理をハンドリングする関数
  Run型を返す。
  こいつはTalkからRunへの自然変換関数となっている。
  Talkを受け取って、具体的な処理を行う。
  返すRunはspeakやlistenと異なりTALKではなくEFFECT。
-}
handleTalk :: forall r. Talk ~> Run (EFFECT + r)
handleTalk = case _ of
  Speak str next -> do
    liftEffect $ log str
    pure next
  Listen reply -> do
    pure (reply "I am Groot")

{-
  TALKのRunを実行する新しいRunを返す
  EFFECTとTALKのRunをEFFECTのRunに変換している
  自然変換関数を使う場合は、interpretを使える(runでもよい)。
  interpret は自然変換関数(VariantF r ~> m)とRun r a を受け取る。
  on p f g r は、 r に p が存在したらfの処理を行い、存在しなかったらgの処理を行う関数。
  on の r は VariantF r2 a という定義で、runTalkの(on _talk handleTalk send)は引数を部分適用(p f gだけ渡している)している状態なので、
  これはVariantF r2 aを受け取って f や g が返す型 b を返すという関数になる。
  なのでinterpretに渡すことができる。
  
  渡されたRunがTalkにマッチする場合の処理はhandleTalkによって行われ、マッチしなかった場合はsendが呼び出される。
-}
runTalk :: forall r. Run (EFFECT + TALK + r) ~> Run (EFFECT + r)
runTalk run = interpret (on _talk handleTalk send) run

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

{-
  eatとcheckPleaseはRunを返す。
  DINNER型を持つRunを返すだけ。
-}
eat :: forall r. Food -> Run (DINNER + r) IsThereMore
eat food = lift _dinner (Eat food identity)

checkPlease :: forall r. Run (DINNER + r) Bill
checkPlease = lift _dinner (CheckPlease identity)

type Tally = { stock :: Int, bill :: Bill }

{-
  渡されたDinner型によって処理をハンドリングする関数
  ↑の方のhandleTalkはRun型を返していたが、こいつはRun型ではなくTupleを返す。
  ただhandleTalkもこいつも、こいつらを使っている関数が返しているのはRun型であるという共通点がある。
  handleTalkは (runTalk :: forall r. Run (EFFECT + TALK + r) ~> Run (EFFECT + r)) で使われていて、
  こいつは↓の   (runDinnerPure :: forall r a. Tally -> Run (DINNER + r) a -> Run r (Tuple Bill a) で使われている。

-}
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

{-
  Tally と DINNERのRun を TupleのRunにして返す
  runTalkと同様にRun型を返すが、runTalkのRunは Run (EFFECT + r)なのに対してこいつは Run r (Tuple Bill a)という違いがある。
  またrunTalkはinterpretを使っているが、こっちはrunAccumPureを使っている。
  runAccumPureを使っているのは、渡したTallyに対して処理を行いたいから。
-} 
runDinnerPure :: forall r a. Tally -> Run (DINNER + r) a -> Run r (Tuple Bill a)
runDinnerPure t run = runAccumPure
  -- この on は vf に _dinner が存在するならばhandleDinnerを呼ぶLoopを返し、存在しないならばDoneを返す
  (\tally vf -> on _dinner (Loop <<< handleDinner tally) Done vf)
  (\tally a -> Tuple tally.bill a)
  t
  run
------------------------------------------------------------------------------

-- TalkとDinnerを合成 ---------------------------------------------------------
type LovelyEvening r = (TALK + DINNER + r)

-- TALKとDINNER両方の関数が呼べる
dinnerTime :: forall r. Run (LovelyEvening r) Unit
dinnerTime = do
  speak "I'm famished!"                  -- speakはTALKのもの
  isThereMore <- eat Pizza               -- eatはDINNERのもの
  if isThereMore then dinnerTime         -- 再帰
  else do
    bill <- checkPlease                  -- checkPleaseはDINNERのもの
    speak $ "Outrageous! " <> show bill  -- speakはTALKのもの

{-
  runTalkはTalkの処理しかハンドリングできないが定義が + r となっているので、Run (TALK + DINNER + r ) を渡すことができる。
  渡すことはできるが、ただしこいつが返してくるRunはDinnerは処理しない。
-}
program :: forall r. Run (EFFECT + DINNER + r) Unit
--program = dinnerTime # runTalk
program = runTalk dinnerTime

{-
  runDinnerPureは、Dinnerしか処理しないが、渡しているprogramはrunTalkでTalkを処理できるので、こいつはDinnerもTalkも処理できる
-}
program2 :: forall r. Run (EFFECT + r) (Tuple Bill Unit)
--program2 = program # runDinnerPure { stock: 10, bill: 0 }
program2 = runDinnerPure { stock: 10, bill: 0 } program

main :: Effect (Tuple Bill Unit)
main = runBaseEffect program2