module Study.Control.Monad.Transformer.Reader where

import Prelude

import Control.Monad.Trans.Class (class MonadTrans)
import Data.Identity (Identity)
import Data.Newtype (class Newtype, unwrap)

-- Reader
type Reader r = ReaderT r Identity

runReader :: forall r a. Reader r a -> r -> a
runReader (ReaderT m) = unwrap <<< m

withReader :: forall r1 r2 a. (r2 -> r1) -> Reader r1 a -> Reader r2 a
withReader = withReaderT
-- Reader

-- Class
class Monad m <= MonadReader r m | m -> r where
  ask :: m r
  local :: forall a. (r -> r) -> m a -> m a

instance monadReader :: MonadReader r ((->) r) where
  ask = identity
  local = (>>>)

asks :: forall r m a. MonadReader r m => (r -> a) -> m a
asks f = f <$> ask
-- Class

-- Transformer

{-
  ReaderT r m a = ReaderT (r -> m a)
  の m はモナドを表し、値としては、(r -> m a)の形の関数を持っている。

  単純なReaderの定義
    newtype Reader r a = Reader (r -> a)
  と比較してみると、形はよく似ている。
  
  大きな違いは、Readerの方は a をそのまま返すのに対して、ReaderTの方は a がモナド m で包まれて返されるところだ。

-}
newtype ReaderT :: forall k. Type -> (k -> Type) -> k -> Type
newtype ReaderT r m a = ReaderT (r -> m a)

runReaderT :: forall r m a. ReaderT r m a -> (r -> m a)
runReaderT (ReaderT x) = x

mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b
mapReaderT f (ReaderT m) = ReaderT (f <<< m)

withReaderT :: forall r1 r2 m a. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
withReaderT f (ReaderT m) = ReaderT (m <<< f)

derive instance newtypeReader :: Newtype (ReaderT r m a) _

instance functorReaderT :: Functor m => Functor (ReaderT r m) where
  map = mapReaderT <<< map

instance applyReaderT :: Apply m => Apply (ReaderT r m) where
  apply (ReaderT f) (ReaderT v) = ReaderT \r -> f r <*> v r

instance applicativeReaderT :: Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT <<< const <<< pure

instance bindReaderT :: Bind m => Bind (ReaderT r m) where
  -- bind (ReaderT m) k = ReaderT \r -> do
  --   a <- (m r)         -- 関数mを適用するとMonadに包まれて返ってくる
  --   case (k a) of      -- 関数kを適用するとReaderTが返ってくる
  --     ReaderT f -> f r -- ReaderTの関数を引数rに適用して返す
  bind :: forall a b r m. Bind m => (ReaderT r m a) -> (a ->  (ReaderT r m b)) -> (ReaderT r m b)
  bind m k = ReaderT \r -> do
    a <- runReaderT m r -- モナドm(ReaderT)を渡して取り出した関数にrを渡し、<-で取り出す。
    runReaderT (k a) r  -- k aの戻り値はReaderTなのでrunReaderTで中身の関数を取り出してrを引数に実行

instance monadReaderTInstance :: Monad m => Monad (ReaderT r m)

instance monadTransReaderT :: MonadTrans (ReaderT r) where
  lift = ReaderT <<< const

instance monadAskReaderT :: Monad m => MonadReader r (ReaderT r m) where
  ask = ReaderT pure
  local = withReaderT
-- Transformer

