module Control.Monad.Reader.MyTrans where

import Prelude

import Control.Monad.Reader.MyClass (class MonadAsk, class MonadReader)
import Data.Newtype (class Newtype)

{-
  r: Read対象の型
  m: 任意のモナド型
  a: 任意のモナド型`m`が扱う任意の型
-}
newtype ReaderT :: forall k. Type -> (k -> Type) -> k -> Type
newtype ReaderT r m a = ReaderT (r -> m a)

-- ReaderTモナド上で計算を実行する
-- 結果は (r -> m a) という関数なので、実際にはこれに r を渡すことで実行する
-- つまり runReaderT readerT r カッコをつけてわかりやすくすると ( (runReaderT readerT) r ) 
-- こうした場合の結果は m a
runReaderT :: forall r m a. ReaderT r m a -> (r -> m a)
runReaderT (ReaderT x) = x

{-
  ReaderTモナドが扱うモナドを別の型に変換する
  r: Read対象の型
  m1: 変換元のモナド型
  a: 変換元のモナドが扱う型
  m2: 変換後のモナド型
  b: 変換後のモナドが扱う型
  ---
  f: モナドを変換する関数
-}
mapReaderT :: forall r m1 m2 a b. (m1 a -> m2 b) -> ReaderT r m1 a -> ReaderT r m2 b
mapReaderT f (ReaderT m) = ReaderT (f <<< m)

{-
  Read対象の型を変換する。
  扱っているモナドの型は変換元と変換後で一致している必要がある。
  r1: 変換後のRead対象の型
  r2: 変換元のRead対象の型
  m: 任意のモナド型
  a: 任意のモナド型`m`が扱う任意の型
  ---
  f: Read対象を変換する関数
-}
withReaderT :: forall r1 r2 m a. (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
withReaderT f (ReaderT m) = ReaderT (m <<< f)

-- ラップ・ラップ解除関数を使用可能にする
derive instance newtypeReaderT :: Newtype (ReaderT r m a) _


instance functorReaderT :: Functor m => Functor (ReaderT r m) where
  map = mapReaderT <<< map

instance applyReaderT :: Apply m => Apply (ReaderT r m) where
  apply (ReaderT f) (ReaderT v) = ReaderT \r -> f r <*> v r

instance applicativeReaderT :: Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT <<< const <<< pure

instance bindReaderT :: Bind monad => Bind (ReaderT reader monad) where
  -- bind :: m a -> (a -> m b) -> m b という形なので、この場合
  -- x は ReaderT, f は ReaderTを返す関数となる。 (a -> ReaderT (r -> m b)) のような形。
  -- f がこのような形なのは、bindの第一引数 m a の a と、第二引数の (a -> m b) の a は任意の型だから。
  -- ただしこの定義上 a はモナド(Bind (ReaderT reader monad) のmonad)の中身の型でなければならない。
  bind x f = ReaderT \r -> 
    -- 環境 r を引数にReaderTモナド x 上で計算を実行。結果はReaderTの(中の)モナドが扱う型の値。
    -- この計算の結果はモナドなので bind (>>=) が使える。つまりbindの中でラップしているモナドのbindを利用している。これが肝。
    (runReaderT x r) >>= 
      -- a は上記モナドの値。それを f に食わせる。
      -- 上述の通り f の結果はReaderTを返す関数なので、rを引数にReaderTモナドの計算を実行して返す。
      -- その結果はまたモナドなので、これにて ReaderT \r -> m b の形が完成する。
      (\a -> runReaderT (f a) r)

instance monadReaderT :: Monad m => Monad (ReaderT r m)

instance monadAskReaderT :: Monad m => MonadAsk r (ReaderT r m) where
  ask = ReaderT pure

instance monadReaderReaderT :: Monad m => MonadReader r (ReaderT r m) where
  local = withReaderT