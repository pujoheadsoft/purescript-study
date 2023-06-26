module Lens.Internal.Forget where

import Prelude

import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple, fst, snd)


-- | `b`の値を忘れ、`a`を返す（累積する）Profunctor
newtype Forget :: forall k. Type -> Type -> k -> Type
newtype Forget r a b = Forget (a -> r) -- `b`は出てこない

derive instance newtypeForget :: Newtype (Forget r a b) _

--derive newtype instance semigroupForget :: Semigroup r => Semigroup (Forget r a b)

--derive newtype instance monoidForget :: Monoid r => Monoid (Forget r a b)


instance profunctorForget :: Profunctor (Forget r) where
  -- dimap の (c -> d) の部分がない。
  -- Forgetは Profunctorの p a c, p b d における c や d を無視しているからこの実装でいける
  dimap :: forall a b c d. (a -> b) -> (c -> d) -> Forget r b c -> Forget r a d
  dimap f _ (Forget z) = Forget (z <<< f)

-- instance choiceForget :: Monoid r => Choice (Forget r) where
--   left  (Forget z) = Forget (either z mempty)
--   right (Forget z) = Forget (either mempty z)

instance strongForget :: Strong (Forget r) where
  first :: forall a b c. Forget r a b -> Forget r (Tuple a c) (Tuple b c)
  first (Forget z) = Forget (z <<< fst) -- Tuple b c は無視されて Tuple a c の a を fst で取って (a -> r) 渡している
  --first (Forget z) = Forget (\(Tuple a _) -> z a) ↑はこういうこと

  second :: forall a b c. Forget r b c -> Forget r (Tuple a b) (Tuple a c)
  second (Forget z) = Forget ( z <<< snd)

-- instance cochoiceForget :: Cochoice (Forget r) where
--   unleft  (Forget z) = Forget (z <<< Left)
--   unright (Forget z) = Forget (z <<< Right)

-- instance wanderForget :: Monoid r => Wander (Forget r) where
--   wander f (Forget r) = Forget (alaF Const f r)