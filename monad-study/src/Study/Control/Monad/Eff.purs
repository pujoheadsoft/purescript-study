module Study.Control.Monad.Eff where

import Prelude

import Data.Exists (Exists, mkExists, runExists)

data EffBindF f a b = EffBindF (Row (f b)) (b -> (Eff f a)) 
data Eff f a =
  Pure a
  | Bind (Exists (EffBindF f a))

instance functorFreer :: Functor (Eff f) where
  map f (Pure a) = Pure $ f a
  map f (Bind e) = runExists (\(EffBindF fa k) -> createBind fa (k >=> Pure <<< f) ) e

instance applyFreer :: Apply (Eff f) where
  apply (Pure fn) a = fn <$> a
  apply (Bind e) a = runExists (\(EffBindF fa k) -> createBind fa (k >=> (_ <$> a)) ) e

instance applicativeFreer :: Applicative (Eff f) where
  pure a = Pure a

instance bindFreer :: Bind (Eff f) where
  bind (Pure a) f = f a
  bind (Bind e) f = runExists (\(EffBindF fa k) -> createBind fa (k >=> f) ) e


createBind :: forall f a b. Row (f b) -> (b -> (Eff f a)) -> Eff f a
createBind m fn = Bind $ mkExists $ EffBindF m fn
