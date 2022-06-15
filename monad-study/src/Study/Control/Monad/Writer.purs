module Study.Control.Monad.Writer where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))


newtype Writer w a = Writer (Tuple a w)

derive instance newtypeWriter :: Newtype (Writer w a) _

instance functorWriter :: Functor (Writer w) where
  map :: forall w a b. (a -> b) -> Writer w a -> Writer w b
  map f (Writer (Tuple a w)) = Writer (Tuple (f a) w)

instance applyWriter :: Semigroup w => Apply (Writer w) where
  apply :: forall a b w. Semigroup w => Writer w (a -> b) -> Writer w a -> Writer w b
  apply (Writer (Tuple f w)) (Writer (Tuple a w')) = Writer (Tuple (f a) (w <> w'))

instance applicativeWriter :: Monoid w => Applicative (Writer w) where
  pure :: forall a w. Monoid w => a -> Writer w a
  pure a = Writer (Tuple a mempty)

instance bindWriter :: Semigroup w => Bind (Writer w) where
  bind :: forall a b w. Semigroup w => Writer w a -> (a -> Writer w b) -> Writer w b
  bind (Writer (Tuple a w)) f = case (f a) of
    Writer (Tuple b w') -> Writer (Tuple b (w <> w'))

instance monadWriterT :: (Monoid w) => Monad (Writer w)

runWriter :: forall w a. Writer w a -> (Tuple a w)
runWriter (Writer w) = w

class (Monoid w, Monad m) <= MonadWriter w m | m -> w where
  tell :: w -> m Unit
  listen :: forall a. m a -> m (Tuple a w)
  pass :: forall a. m (Tuple a (w -> w)) -> m a

--
-- 上記定義の Monad m の部分は Writer になる。
-- Monoid w の部分は任意だが、WriterがMonoid wを持っていることとする。
-- (MonadWriter w (Writer w) は、MonadWriterという型が w と (Writer w) を持っているわけではない。あくまで定義。)
-- tellやlistenなどの関数にシグニチャは書く必要がないが、理解のためWriterなどをあてはめて書いた。
--
instance monadWriter :: Monoid w => MonadWriter w (Writer w) where
  tell :: forall w. Monoid w => w -> Writer w Unit
  tell a = Writer (Tuple unit a)

  listen :: forall a w. Writer w a -> Writer w (Tuple a w)
  listen (Writer (Tuple a w)) = Writer (Tuple (Tuple a w) w)

  --pass :: forall a w. Monoid w => Writer (Tuple a (w -> w)) w -> Writer w a
  pass (Writer (Tuple (Tuple a f) w)) = Writer (Tuple a (f w))