module Study.Control.Monad.Run.Writer where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (on)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Study.Control.Monad.Run.Run (Run, lift, peel, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

data Writer w a = Writer w a

derive instance functorWriter :: Functor (Writer w)

type WRITER w r = (writer :: Writer w | r)

_writer :: Proxy "writer"
_writer = Proxy

liftWriter :: forall w a r. Writer w a -> Run (WRITER w + r) a
liftWriter = liftWriterAt _writer

writer :: forall w a r. (Tuple a w) -> Run (WRITER w + r) a
writer (Tuple a w) = liftWriter (Writer w a)

liftWriterAt
  :: forall w a r t s
   . IsSymbol s
   => Row.Cons s (Writer w) t r
   => Proxy s
   -> Writer w a
   -> Run r a
liftWriterAt = lift

runWriter :: forall w a r. Monoid w => Run (WRITER w + r) a -> Run r (Tuple a w)
runWriter = runWriterAt _writer

runWriterAt
  :: forall w a r t s
   . IsSymbol s
  => Monoid w
  => Row.Cons s (Writer w) t r
  => Proxy s
  -> Run r a
  -> Run t (Tuple a w)
runWriterAt sym = foldWriterAt sym (<>) mempty

foldWriter :: forall w b a r. (b -> w -> b) -> b -> Run (WRITER w + r) a -> Run r (Tuple a b)
foldWriter = foldWriterAt _writer

foldWriterAt
  :: forall w b a r t s
   . IsSymbol s
  => Row.Cons s (Writer w) t r
  => Proxy s
  -> (b -> w ->b)
  -> b
  -> Run r a
  -> Run t (Tuple a b)
foldWriterAt sym = loop
  where
  handle = on sym Left Right
  loop k w r = case peel r of
    Left a -> case handle a of
      Left (Writer w' n) ->
        loop k (k w w') n
      Right a' ->
        send a' >>= foldWriterAt sym k w
    Right a ->
      pure (Tuple a w)


tell :: forall w r. w -> Run (writer :: Writer w | r) Unit
tell = tellAt _writer

tellAt
  :: forall w r t s
   . IsSymbol s
  => Row.Cons s (Writer w) t r
  => Proxy s
  -> w
  -> Run r Unit
tellAt sym w = liftWriterAt sym (Writer w unit)

censor :: forall w a r. (w -> w) -> Run (writer :: Writer w | r) a -> Run (writer :: Writer w | r) a
censor = censorAt _writer

censorAt
  :: forall w a r t s
   . IsSymbol s
  => Row.Cons s (Writer w) t r
  => Proxy s
  -> (w -> w)
  -> Run r a
  -> Run r a
censorAt sym = loop
  where
  handle = on sym Left Right
  loop f r = case peel r of
    Left a -> case handle a of
      Left (Writer w n) -> do
        tellAt sym (f w)
        loop f n
      Right _ ->
        send a >>= loop f
    Right a ->
      pure a