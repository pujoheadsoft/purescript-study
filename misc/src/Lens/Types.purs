module Lens.Types where

import Prelude

import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Lens.Internal.Forget (Forget)
import Lens.Internal.Tagged (Tagged)

{-
  type Lens s t a b    = forall p. Strong p => p a b -> p s t
  type Lens' s a       = forall p. Strong p => p a a -> p s s
  type Prism s t a b   = forall p. Choice p => p a b -> p s t
  type Prism' s a      = forall p. Choice p => p a a -> p s s
  type Getter s t a b  = forall r. Forget r a b -> Forget r s t
  type Getter' s a     = forall r. Forget r a a -> Forget r s s
  type AGetter s t a b = Forget a a b -> Forget a s t
  type AGetter' s a    = Forget a a a -> Forget a s s
  type Setter s t a b  = (a -> b) -> (s -> t)
  type Review s t a b  = Tagged b -> Tagged t
-}

-- | Lens
-- | 実体はOpticなのでこういう構造
-- |   p a b
-- | → p s t
-- | p は Strong なので *** や &&& が使えるよ
-- | *** は p a b -> p s t -> p (Tuple a s) (Tuple b t)
-- | &&& は p a b -> p a c -> p a (Tuple b c)
type Lens s t a b = forall p. Strong p => Optic p s t a b

-- | 特殊化されたLens
-- |   p a a
-- | → p s s
type Lens' s a = Lens s s a a

-- | Prism
-- | Lensと同じく実体はOpticなのでこういう構造
-- |   p a b
-- | → p s t
-- | p は Choice なので +++ や ||| が使えるよ
-- | +++ は p a b -> p s t -> p (Either a s) (Either b t)
-- | ||| は p a c -> p b c -> p (Either a b) c
type Prism s t a b = forall p. Choice p => Optic p s t a b

-- | 特殊化されたPrism
-- |   p a a
-- | → p s s
type Prism' s a = Prism s s a a

-- | 基本のtype。LensとかPrismとかの中身は全部この構造
-- |   p a b
-- | → p s t
type Optic :: (Type -> Type -> Type) -> Type -> Type -> Type -> Type -> Type
type Optic p s t a b = p a b -> p s t

-- | An affine traversal
-- | forall p. Strong p => Choice p => p a b -> p s t
type AffineTraversal s t a b = forall p. Strong p => Choice p => Optic p s t a b
type AffineTraversal' s a = AffineTraversal s s a a


-- | getter.
-- |
-- | Foldの定義から展開してみるとこうなる
-- | Forget r a b -> Forget r s t
-- | ↓
-- | Forget (a -> r) -> Forget (s -> r)
type Getter :: Type -> Type -> Type -> Type -> Type
type Getter s t a b = forall r. Fold r s t a b

-- | Getterじゃなくてこっちでよさそう
-- | Forget r a a -> Forget r s s
-- | Forget (a -> r) -> Forget (s -> r)
type Getter' s a = Getter s s a a

-- | A getter.
-- |
-- | Foldの定義から展開してみるとこうなる
-- | Forget a a b -> Forget a s t
-- | ↓
-- | Forget (a -> a) -> Forget (s -> a)
type AGetter :: Type -> Type -> Type -> Type -> Type
type AGetter s t a b = Fold a s t a b

type AGetter' s a = AGetter s s a a

-- | A Setter
-- | 実体はOpticなのでこういう構造
-- |   p a b  → p s t
-- | 更に`Function`なのでこうなる
-- |   (a -> b) -> (s -> t)
type Setter s t a b = Optic Function s t a b

-- | (a -> a) -> (s -> s)
type Setter' s a = Setter s s a a

-- | A review.
-- | Taggedの定義は以下
-- | Tagged a b = Tagged b
-- |
-- | なのでこうなる(`s`や`a`は無視される)
-- | Review s t a b = Tagged b -> Tagged t
type Review :: Type -> Type -> Type -> Type -> Type
type Review s t a b = Optic Tagged s t a b

type Review' s a = Review s s a a

-- | A fold.
-- | 
-- | Optic と Forget の定義は以下
-- | type = Optic p s t a b = p a b -> p s t
-- | newtype Forget r a b = Forget (a -> r)
-- |
-- | 上記をもとに定義を展開すると Fold は以下のようになる
-- | Forget r a b -> Forget r s t
-- |
-- | つまり内容的にはこう
-- | Forget (a -> r) -> Forget (s -> r)
-- |
-- | 型の部分を無視してみるとこういう構造が見えてくる
-- | (a -> r) -> (s -> r)
type Fold :: Type -> Type -> Type -> Type -> Type -> Type
type Fold r s t a b = Optic (Forget r) s t a b




