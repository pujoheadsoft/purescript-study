module Study.Control.Monad.LawBroken.LawBroken where

import Prelude

newtype LawBroken r a = LawBroken { r :: r, a :: a }

instance functorLawBroken :: Ring r => Functor (LawBroken r) where
  map :: forall a b. (a -> b) -> LawBroken r a -> LawBroken r b
  map f (LawBroken {r, a}) = LawBroken {r: r, a: (f a)}

instance applyLawBroken :: Ring r => Apply (LawBroken r) where
  apply :: forall a b. LawBroken r (a -> b) -> LawBroken r a -> LawBroken r b
  apply (LawBroken {r: r1, a: f}) (LawBroken {r: r2, a: a}) = LawBroken {r: r1 - r2, a: f a}

instance bindLawBroken :: Ring r => Bind (LawBroken r) where
  bind :: forall a b. LawBroken r a -> (a -> LawBroken r b) -> LawBroken r b
  bind (LawBroken {r, a}) f = do
    let LawBroken {r: r', a: a'} = f a
    LawBroken {r: r - r', a: a'}

instance applicativeLawBroken :: Ring r => Applicative (LawBroken r) where
  pure :: forall a. a -> LawBroken r a
  pure a = LawBroken {r: zero, a: a}

instance monadLawBroken :: Ring r => Monad (LawBroken r)

instance showLawBroken :: (Show r, Show a) => Show (LawBroken r a) where
  show (LawBroken {r, a}) = "LawBroken {r: " <> show r <> ", a: " <> show a <> "}"

instance eqLawBroken :: (Eq r, Eq a) => Eq (LawBroken r a) where
  eq (LawBroken {r: r1, a: a1}) (LawBroken {r: r2, a: a2}) = r1 == r2 && a1 == a2

