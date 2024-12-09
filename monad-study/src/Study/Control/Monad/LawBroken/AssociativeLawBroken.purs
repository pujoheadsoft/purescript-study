module Study.Control.Monad.LawBroken.AssociativeLawBroken where

import Prelude

newtype AssociativeLawBroken r a = AssociativeLawBroken { r :: r, a :: a }

instance Ring r => Functor (AssociativeLawBroken r) where
  map f (AssociativeLawBroken {r, a}) = AssociativeLawBroken {r: r, a: (f a)}

instance Ring r => Apply (AssociativeLawBroken r) where
  apply (AssociativeLawBroken {r: r1, a: f}) (AssociativeLawBroken {r: r2, a: a}) = AssociativeLawBroken {r: r1 - r2, a: f a}

instance Ring r => Bind (AssociativeLawBroken r) where
  bind (AssociativeLawBroken {r, a}) f = do
    let AssociativeLawBroken {r: r', a: a'} = f a
    AssociativeLawBroken {r: r - r', a: a'}

instance Ring r => Applicative (AssociativeLawBroken r) where
  pure a = AssociativeLawBroken {r: zero, a: a}

instance Ring r => Monad (AssociativeLawBroken r)

instance (Show r, Show a) => Show (AssociativeLawBroken r a) where
  show (AssociativeLawBroken {r, a}) = "AssociativeLawBroken {r: " <> show r <> ", a: " <> show a <> "}"

instance (Eq r, Eq a) => Eq (AssociativeLawBroken r a) where
  eq (AssociativeLawBroken {r: r1, a: a1}) (AssociativeLawBroken {r: r2, a: a2}) = r1 == r2 && a1 == a2

