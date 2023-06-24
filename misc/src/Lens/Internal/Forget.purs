module Lens.Internal.Forget where

import Prelude

import Data.Const (Const(..))
import Data.Either (Either(..), either)
import Data.Newtype (class Newtype, alaF)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Cochoice (class Cochoice)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (fst, snd)
import Lens.Internal.Wander (class Wander)


-- | `b`の値を忘れ、`a`を返す（累積する）Profunctor
newtype Forget :: forall k. Type -> Type -> k -> Type
newtype Forget r a b = Forget (a -> r) -- `b`は出てこない

derive instance newtypeForget :: Newtype (Forget r a b) _

derive newtype instance semigroupForget :: Semigroup r => Semigroup (Forget r a b)

derive newtype instance monoidForget :: Monoid r => Monoid (Forget r a b)


instance profunctorForget :: Profunctor (Forget r) where
  -- dimap の (c -> d) の部分がない。
  -- Forgetは Profunctorの p a c, p b d における c や d がないからこの実装でいける
  dimap f _ (Forget z) = Forget (z <<< f)

-- instance choiceForget :: Monoid r => Choice (Forget r) where
--   left  (Forget z) = Forget (either z mempty)
--   right (Forget z) = Forget (either mempty z)

instance strongForget :: Strong (Forget r) where
  first (Forget z) = Forget (z <<< fst)
  second (Forget z) = Forget ( z <<< snd)

-- instance cochoiceForget :: Cochoice (Forget r) where
--   unleft  (Forget z) = Forget (z <<< Left)
--   unright (Forget z) = Forget (z <<< Right)

-- instance wanderForget :: Monoid r => Wander (Forget r) where
--   wander f (Forget r) = Forget (alaF Const f r)