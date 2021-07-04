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

instance bindReaderT :: Bind m => Bind (ReaderT r m) where
  -- k はモナド ReaderT の内容 m を引数にとり、モナド ReaderT を返す関数。
  -- bindの定義は forall a b. m a -> (a -> m b) -> m b なので
  -- k には m(↑の定義でいうa)を渡したいが、 m は(r -> monad a)という関数であるため直接は渡せない。
  bind (ReaderT m) k = ReaderT -- ReaderTを返す。その内容は以下関数。
    -- 環境rを受け取ってまずはm(r -> monad a)にrを渡す。結果はmonadになっているので、bindが使える。
    (\r -> m r >>= 
      -- ↑の(m r)の結果が引数 a になっている。ここでようやく↑の k を適用できる。
      -- k の結果は ReaderT になるので、パターンマッチ。そして ReaderTの値は環境を受け取る関数なので、(f r)として適用。
      \a -> case k a of ReaderT f -> f r)

instance monadReaderT :: Monad m => Monad (ReaderT r m)

instance monadAskReaderT :: Monad m => MonadAsk r (ReaderT r m) where
  ask = ReaderT pure

instance monadReaderReaderT :: Monad m => MonadReader r (ReaderT r m) where
  local = withReaderT