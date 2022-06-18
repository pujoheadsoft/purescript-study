module Study.Control.Monad.Transformer.Writer where

import Prelude

import Data.Identity (Identity)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), snd)

-- Writer --
type Writer w = WriterT w Identity

writer :: forall w a. Tuple a w -> Writer w a
writer = WriterT <<< pure

runWriter :: forall w a. Writer w a -> Tuple a w
runWriter = unwrap <<< runWriterT
-- Writer --

-- Class --
class (Semigroup w, Monad m) <= MonadTell w m | m -> w where
  tell :: w -> m Unit

class (Monoid w, MonadTell w m) <= MonadWriter w m | m -> w where
  listen :: forall a. m a -> m (Tuple a w)
  pass :: forall a. m (Tuple a (w -> w)) -> m a

listens :: forall w m a b. MonadWriter w m => (w -> b) -> m a -> m (Tuple a b)
listens f m = do
  Tuple a w <- listen m
  pure $ Tuple a (f w)

censor :: forall w m a. MonadWriter w m => (w -> w) -> m a -> m a
censor f m = pass do
  a <- m
  pure $ Tuple a f
-- Class --

-- Transformer --
{-
  WriterT w m a = WriterT (m (Tuple a w))
  の m はモナド。

  単純なWriterの定義
  newtype Writer w a = Writer (Tuple a w)
  と比べると、Tupleがモナドに包まれていることがわかる
-}
newtype WriterT w m a = WriterT (m (Tuple a w))


runWriterT :: forall w m a. WriterT w m a -> m (Tuple a w)
runWriterT (WriterT x) = x

execWriterT :: forall w m a. Functor m => WriterT w m a -> m w
execWriterT (WriterT m) = snd <$> m -- Tupleのsnd関数でmap

mapWriterT :: forall w1 w2 m1 m2 a b. (m1 (Tuple a w1) -> m2 (Tuple b w2)) -> WriterT w1 m1 a -> WriterT w2 m2 b
mapWriterT f (WriterT m) = WriterT (f m)


derive instance newtypeWriterT :: Newtype (WriterT w m a) _

instance functorWriter :: Functor m => Functor (WriterT w m) where
  map :: forall a b. (a -> b) -> WriterT w m a -> WriterT w m b
  map f w = mapWriterT (map \(Tuple a w') -> Tuple (f a) w') w
  -- mapWriterTに渡さなければならないのは、(m (Tuple a w))を(m (Tuple b w))に変換する関数
  -- つまりmに対してのmap関数なのでmapを渡している。

instance applyWriterT :: (Semigroup w, Apply m) => Apply (WriterT w m) where
  apply (WriterT f) (WriterT v) = WriterT
    let 
      k :: forall a b. (Tuple (a -> b) w) -> (Tuple a w) -> (Tuple b w)
      k (Tuple g w) (Tuple a w') = Tuple (g a) (w <> w')
    in k <$> f <*> v

instance applicativeWriterT :: (Monoid w, Applicative m) => Applicative (WriterT w m) where
  pure a = WriterT $ pure $ Tuple a mempty

instance bindWriterT :: (Semigroup w, Bind m) => Bind (WriterT w m) where
  bind :: forall a b w m. Semigroup w => Bind m => WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
  bind (WriterT m) k = WriterT do
    (Tuple a w) <- m
    case k a of 
      WriterT wt -> map (\(Tuple b w') -> Tuple b (w <> w')) wt

instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)


instance monadTellWriterT :: (Monoid w, Monad m) => MonadTell w (WriterT w m) where
  tell = WriterT <<< pure <<< Tuple unit

instance monadWriterWriterT :: (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  listen (WriterT m) = WriterT do
    Tuple a w <- m
    pure $ Tuple (Tuple a w) w
  
  pass (WriterT m) = WriterT do
    Tuple (Tuple a f) w <- m
    pure $ Tuple a (f w)
-- Transformer --