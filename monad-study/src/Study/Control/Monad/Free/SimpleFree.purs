module Study.Control.Monad.Free.SimpleFree where

{-
  現行の purescript-free はかなり複雑なことになってるので単純だった頃の実装を写経してみる
  https://github.com/purescript/purescript-free/blob/0.0.6/src/Control/Monad/Free.purs
  写経した後、多少コードは変えた。
  最低限必要な状態を考えたかったので、必要最低限なところとそれ以外を(コメントで)分けてる。
-}

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans)

-- 再帰的な定義になっている
data Free f a
  = Pure a
  | Free (f (Free f a))

instance functorFree :: (Functor f) => Functor (Free f) where
  map f (Pure a) = Pure (f a)
  map f (Free fa) = Free (map f <$> fa) -- 元々の`f`はそのまま使って、再帰。ただし値は`fa`。結局`Pure`にパターンマッチするまで再帰することになる。

instance applyFree :: (Functor f) => Apply (Free f) where
  apply = ap -- apはMonadであれば使える関数

instance applicativeFree :: (Functor f) => Applicative (Free f) where
  pure = Pure

instance bindFree :: (Functor f) => Bind (Free f) where
  bind (Pure a) f = f a
  -- ここでFunctorを利用している
  -- Freeの中にFreeがあるようなネスト構造の場合、そのFreeの型もFunctorであるので、再帰的にbind関数を呼び出せる。
  bind (Free m) f = Free ((\a -> a >>= f) <$> m)

instance monadFree :: (Functor f) => Monad (Free f)

liftF :: forall f. Functor f => f ~> Free f
liftF fa = Free $ pure <$> fa

-- ここまでは最低必要かな(あとはあると便利なやつ)

foldFree :: forall f m. MonadRec m => (f ~> m) -> Free f ~> m
foldFree k = tailRecM go
  where
  go :: forall a. Free f a -> m (Step (Free f a) a)
  go f = case f of
    Pure a -> Done <$> pure a
    Free g -> Loop <$> k g


instance monadTransFree :: MonadTrans Free where
  lift f = Free $ do
    a <- f
    pure (Pure a)

class MonadFree :: forall k. (Type -> Type) -> (k -> Type) -> Constraint
class MonadFree f m where
  wrap :: forall a. f (m a) -> m a

instance monadFreeFree :: (Functor f) => MonadFree f (Free f) where
  wrap = Free

-- 元の実装 MonadFreeの制約はなくてもいいので別名にして残した
liftF' :: forall f m a. Functor f => Monad m => MonadFree f m => f a -> m a
liftF' fa = wrap $ pure <$> fa

iterM :: forall f m a. Functor f => Monad m => (f (m a) -> m a) -> Free f a -> m a
iterM _ (Pure a) = pure a
iterM k (Free f) = k $ (iterM k) <$> f
