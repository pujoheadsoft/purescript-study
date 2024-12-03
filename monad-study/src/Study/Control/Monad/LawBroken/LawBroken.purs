module Study.Control.Monad.LawBroken.LawBroken where

import Prelude

newtype LawBroken r a = LawBroken { r :: r, a :: a }

instance functorLawBroken :: Functor (LawBroken r) where
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

broken :: forall r. Ring r => r -> LawBroken r Unit
broken r = LawBroken {r: r, a: unit}

value :: forall r. Ring r => LawBroken r Unit -> r
value (LawBroken {r}) = r