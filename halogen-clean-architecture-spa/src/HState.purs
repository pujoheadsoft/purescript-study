module HState where

import Prelude

import Control.Monad.State (class MonadState)
import Control.Monad.State as MS
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Prim.Row as Row
import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

{-
  型は Run 型としたい。
  その型の modify は H.modify を呼ぶものとしたい。
  modify の定義は MonadState s m => (s -> s) -> m s となっている。
  m は Monad で m -> s という関数従属性を持っている。

  既存の Run (STATE) を利用してどうにかならないか？
-}


data State s a = State (s -> s) (s -> a)

derive instance functorState :: Functor (State s)

type STATE s r = (state :: State s | r)

_state :: Proxy "state"
_state = Proxy

liftState :: forall s a r. State s a -> Run (STATE s + r) a
liftState = liftStateAt _state

liftStateAt
  :: forall q sym s a r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> State s a
  -> Run r a
liftStateAt = Run.lift

modify :: forall s r. (s -> s) -> Run (STATE s + r) Unit
modify = modifyAt _state

modifyAt
  :: forall q sym s r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> (s -> s)
  -> Run r Unit
modifyAt sym f = liftStateAt sym $ State f (const unit)

modify2 :: forall s r m. MonadState s m => (s -> s) -> Run (STATE s + r) (m s)
modify2 = modifyAt2 _state

modifyAt2
  :: forall q sym s r m
   . MonadState s m
  => IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> (s -> s)
  -> Run r (m s)
modifyAt2 sym f = liftStateAt sym $ State f (const $ MS.modify f)

modify3 :: forall s r m. MonadState s m => (s -> s) -> Run (STATE s + r) (m Unit)
modify3 = modifyAt3 _state

modifyAt3
  :: forall q sym s r m
   . MonadState s m
  => IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> (s -> s)
  -> Run r (m Unit)
modifyAt3 sym f = liftStateAt sym $ State f (const $ MS.modify_ f)

put :: forall s r. s -> Run (STATE s + r) Unit
put = putAt _state

putAt
  :: forall q sym s r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> s
  -> Run r Unit
putAt sym = modifyAt sym <<< const

get :: forall s r. Run (STATE s + r) s
get = getAt _state

getAt
  :: forall q sym s r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> Run r s
getAt sym = liftStateAt sym $ State identity identity

gets :: forall s t r. (s -> t) -> Run (STATE s + r) t
gets = getsAt _state

getsAt
  :: forall q sym s t r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> (s -> t)
  -> Run r t
getsAt sym = flip map (getAt sym)

runState :: forall s r a. s -> Run (STATE s + r) a -> Run r (Tuple s a)
runState = runStateAt _state

runStateAt
  :: forall q sym s r a
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> s
  -> Run r a
  -> Run q (Tuple s a)
runStateAt sym = loop
  where
  handle = Run.on sym Left Right
  loop s r = case Run.peel r of
    Left a -> case handle a of
      Left (State t k) ->
        let
          s' = t s
        in
          loop s' (k s')
      Right a' ->
        Run.send a' >>= runStateAt sym s
    Right a ->
      pure (Tuple s a)

evalState :: forall s r a. s -> Run (STATE s + r) a -> Run r a
evalState = evalStateAt _state

evalStateAt
  :: forall q sym s r a
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> s
  -> Run r a
  -> Run q a
evalStateAt sym s = map snd <<< runStateAt sym s

execState :: forall s r a. s -> Run (STATE s + r) a -> Run r s
execState = execStateAt _state

execStateAt
  :: forall q sym s r a
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> s
  -> Run r a
  -> Run q s
execStateAt sym s = map fst <<< runStateAt sym s