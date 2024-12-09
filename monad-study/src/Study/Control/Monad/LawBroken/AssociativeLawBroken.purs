module Study.Control.Monad.LawBroken.AssociativeLawBroken where

import Prelude

newtype AssociativeLawBroken v a = AssociativeLawBroken { value :: v, a :: a }

instance Ring r => Functor (AssociativeLawBroken r) where
  map f (AssociativeLawBroken {value, a}) = AssociativeLawBroken {value, a: (f a)}

instance Ring r => Apply (AssociativeLawBroken r) where
  apply (AssociativeLawBroken {value: v1, a: f}) (AssociativeLawBroken {value: v2, a: a})
    = AssociativeLawBroken {value: v1 - v2, a: f a}

instance Ring r => Bind (AssociativeLawBroken r) where
  bind (AssociativeLawBroken {value: v, a}) f = do
    let AssociativeLawBroken {value: v', a: a'} = f a
    AssociativeLawBroken {value: v - v', a: a'}

instance Ring r => Applicative (AssociativeLawBroken r) where
  pure a = AssociativeLawBroken {value: zero, a: a}

instance Ring r => Monad (AssociativeLawBroken r)

instance (Show r, Show a) => Show (AssociativeLawBroken r a) where
  show (AssociativeLawBroken {value, a}) = "{value: " <> show value <> ", a: " <> show a <> "}"

instance (Eq r, Eq a) => Eq (AssociativeLawBroken r a) where
  eq (AssociativeLawBroken {value: v1, a: a1}) (AssociativeLawBroken {value: v2, a: a2}) = v1 == v2 && a1 == a2

