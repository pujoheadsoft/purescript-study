module Study.Control.Monad.Run.State where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (on)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Prim.Row as Row
import Study.Control.Monad.Run.Run (Run, lift, peel, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data State s a = State (s -> s) (s -> a)

derive instance functorState :: Functor (State s)

type STATE s r = (state :: State s | r)

_state :: Proxy "state"
_state = Proxy

state :: forall s a r. (s -> a) -> Run (STATE s + r) a
state f = liftState (State identity f)

liftState :: forall s a r. State s a -> Run (STATE s + r) a
liftState = liftStateAt _state

liftStateAt
  :: forall q sym s a r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> State s a
  -> Run r a
liftStateAt = lift

runState :: forall s r a. Run (STATE s + r) a -> s -> Run r (Tuple a s)
runState r s = runStateAt _state s r

runStateAt
  :: forall q sym s r a
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> s
  -> Run r a
  -> Run q (Tuple a s)
runStateAt sym = loop
  where
  handle = on sym Left Right
  loop s r = case peel r of
    Left a -> case handle a of
      Left (State t k) ->
        let s' = t s
        in loop s' (k s')
      Right a' ->
        send a' >>= runStateAt sym s
    Right a ->
      pure (Tuple a s)


evalState :: forall s r a. Run (STATE s + r) a -> s -> Run r a
evalState r s = evalStateAt _state s r

evalStateAt
  :: forall q sym s r a
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> s
  -> Run r a
  -> Run q a
evalStateAt sym s = map fst <<< runStateAt sym s

execState :: forall s r a. Run (STATE s + r) a -> s -> Run r s
execState r s = execStateAt _state s r

execStateAt
  :: forall q sym s r a
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Proxy sym
  -> s
  -> Run r a
  -> Run q s
execStateAt sym s = map snd <<< runStateAt sym s


get :: forall s r. Run (STATE s + r) s
get = state identity

gets :: forall s t r. (s -> t) -> Run (STATE s + r) t
gets = flip map get

modify :: forall s r. (s -> s) -> Run (STATE s + r) Unit
modify f = liftState $ State f (const unit)

put :: forall s r. s -> Run (STATE s + r) Unit
put = modify <<< const