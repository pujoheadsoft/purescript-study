module Lens.Internal.Wander where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (class Newtype)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong)
import Safe.Coerce (class Coercible, coerce)

-- | 多相なTraversalをサポートするProfunctorのクラス
class (Strong p, Choice p) <= Wander p where
  wander
    :: forall s t a b
     . (forall f. Applicative f => (a -> f b) -> s -> f t)
     -> p a b
     -> p s t

instance wanderFunction :: Wander Function where
  wander t pab = alaF Identity t pab

instance wanderStar :: Applicative f => Wander (Star f) where
  wander t (Star f) = Star (t f)


{-
  ``` purescript
  alaF Additive foldMap String.length ["hello", "world"] -- 10
  alaF Multiplicative foldMap Math.abs [1.0, -2.0, 3.0, -4.0] -- 24.0
  ```
  Coercible a b は a から b に安全に coerce できることを保証する型クラス
  なので以下が可能。
  f t -> f a
  g s -> g b
-}
alaF
  :: forall f g t a s b
   . Coercible (f t) (f a)
  => Coercible (g s) (g b)
  => Newtype t a
  => Newtype s b
  => (a -> t)     -- wrapするような関数だが無視されてる
  -> (f t -> g s) 
  -> f a
  -> g b
alaF _ ft2gs fa = (coerce ft2gs) fa