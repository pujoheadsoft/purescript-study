module Study.Control.Monad.SimpleFree where

-- 現行の purescript-free はかなり複雑なことになってるので単純だった頃の実装を写経してみる
-- reference https://github.com/purescript/purescript-free/blob/0.0.6/src/Control/Monad/Free.purs

import Prelude
import Data.Tuple (Tuple(..))
import Control.Monad.Trans.Class

-- 再帰的な定義になっている
data Free f a = 
  Pure a 
  | Free (f (Free f a))

class MonadFree f m where
  wrap :: forall a. f (m a) -> m a

instance functorFree :: (Functor f) => Functor (Free f) where
  map f = go where
    go (Pure a) = Pure (f a)
    go (Free fa) = Free (go <$> fa)

instance applyFree :: (Functor f) => Apply (Free f) where
  apply = ap

instance applicativeFree :: (Functor f) => Applicative (Free f) where
  pure = Pure

instance bindFree :: (Functor f) => Bind (Free f) where
  bind (Pure a) f = f a
  bind (Free m) f = Free ((<$>) (\a -> a >>= f) m)

instance monadFree :: (Functor f) => Monad (Free f)

instance monadTransFree :: MonadTrans Free where
  lift f = Free $ do
    a <- f
    pure (Pure a)

instance monadFreeFree :: (Functor f) => MonadFree f (Free f) where
  wrap = Free

liftF :: forall f m a. Functor f => Monad m => MonadFree f m => f a -> m a
liftF fa = wrap $ pure <$> fa

iterM :: forall f m a. Functor f => Monad m => (f (m a) -> m a) -> Free f a -> m a
iterM _ (Pure a) = pure a
iterM k (Free f) = k $ (iterM k) <$> f
