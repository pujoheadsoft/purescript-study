module Data.Lens.Types where

import Prelude

import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)


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