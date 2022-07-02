module Example.Run.Example where

import Prelude

import Control.Monad.Rec.Class (Step(..))
import Data.Array (cons)
import Data.Either (Either(..))
import Data.Functor.Variant (on)
import Data.Symbol (class IsSymbol)
import Debug (traceM)
import Effect (Effect)
import Prim.Row as Row
import Study.Control.Monad.Run.Run (Run, extract, lift, peel, runPure, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

{-
  自前で作ってみた例
  run関数にhandlerを渡せるようにしたり、runを色々書いてみたり
-}

type User = {id :: String, name :: String}

-- aは継続。(User -> a)にように継続に変換できるようにしているのがミソ
data FindUser a
  = FindUserById String (User -> a)
  | FindUserByName String (Array User -> a)

derive instance functorFindUser :: Functor FindUser

type FIND_USER r = (findUser :: FindUser | r)

_findUser = Proxy :: Proxy "findUser"

findUserById :: forall r. String -> Run (FIND_USER + r) User
findUserById id = lift _findUser (FindUserById id identity) -- Userに手を加えないのでidentityでよい

findUserByName :: forall r. String -> Run (FIND_USER + r) (Array User)
findUserByName name = lift _findUser (FindUserByName name identity)

-- handler。こいつは差し替えられる。
handleFindUser :: forall a. FindUser a -> a
handleFindUser (FindUserById id reply) = reply {id: id, name: "dummy"}
handleFindUser (FindUserByName name reply) = reply [{id: "dummy1" , name: name}, {id: "dummy2" , name: name}]

-- ここからrun関数 -------------------------------------------------------

-- handlerを直接呼ぶ版
runFindUser :: forall r a. Run (FIND_USER + r) a -> Run r a
runFindUser r = case peel r of
  Left a -> case on _findUser Left Right a of
    Left a' -> runFindUser (handleFindUser a')
    Right a' -> send a' >>= \r' -> runFindUser r'
  Right a ->
    pure a

-- handlerを渡せるようにした版
runFindUser2 :: 
  forall r a
   . (FindUser (Run (FIND_USER + r) a) -> Run (FIND_USER + r) a)
  -> Run (FIND_USER + r) a
  -> Run r a
runFindUser2 f r = case peel r of
  Left a -> case on _findUser Left Right a of
    Left a' -> runFindUser2 f (f a')
    Right a' -> send a' >>= \r' -> runFindUser2 f r'
  Right a ->
    pure a

-- handlerを直接渡せるようにし、かつReaderとかと同じ書き方(At関数を分け、whereで別関数を作る)をした版
runFindUser3 :: 
  forall r a
   . (FindUser (Run (FIND_USER + r) a) -> Run (FIND_USER + r) a)
  -> Run (FIND_USER + r) a
  -> Run r a
runFindUser3 f r = runFindUserAt _findUser f r

runFindUserAt ::
  forall t a r s
  . IsSymbol s
  => Row.Cons s FindUser t r
  => Proxy s
  -> (FindUser (Run r a) -> Run r a)
  -> Run r a
  -> Run t a
runFindUserAt sym f = loop
  where
    handle = on sym Left Right
    loop r = case peel r of
      Left a -> case handle a of
        Left a' -> loop (f a')
        Right a' -> send a' >>= \r' -> runFindUserAt sym f r'
      Right a ->
        pure a

-- handlerを渡せて、かつ実装を短くした版
runFindUser4 :: 
  forall r a
   . (FindUser (Run (FIND_USER + r) a) -> Run (FIND_USER + r) a)
  -> Run (FIND_USER + r) a
  -> Run r a
runFindUser4 f r = runPure (\v -> on _findUser (Loop <<< f) Done v) r
-- runPureの第一引数は関数で、VariantFを受け取ってStepを返す関数
-- _finderがあったら関数`f`をかましてLoopを返す。なかったらDoneを返す
-- runPureの中身を見ると、この実装はrunFindUser2とほとんど同じ意味合いであることがわかる

-- 実際に処理を呼び出す関数(これを呼んだタイミングでは実行されない)
program :: forall r. String -> Run (FIND_USER + r) (Array User)
program id = do
  user <- findUserById id
  users <- findUserByName user.id
  pure $ cons user users

main :: Effect Unit
main = do
  traceM(extract (runFindUser4 handleFindUser (program "userId")))
   