module Study.Control.Monad.Freer.Freer where

import Prelude

import Data.Exists (Exists, mkExists, runExists)

-- |
-- | Freer Monad
-- | 
-- | Freer は Functorがなくても作れるFreeモナド
-- |

data FreerBindF f a b = FreerBindF (f b) (b -> (Freer f a)) 
data Freer f a =
  Pure a
  | Bind (Exists (FreerBindF f a))

instance functorFreer :: Functor (Freer f) where
  map f (Pure a) = Pure $ f a
  map f (Bind e) = runExists (\(FreerBindF fa k) -> createBind fa (k >=> Pure <<< f) ) e

instance applyFreer :: Apply (Freer f) where
  apply (Pure fn) a = fn <$> a
  apply (Bind e) a = runExists (\(FreerBindF fa k) -> createBind fa (k >=> (_ <$> a)) ) e

instance applicativeFreer :: Applicative (Freer f) where
  pure a = Pure a

instance bindFreer :: Bind (Freer f) where
  bind (Pure a) f = f a
  bind (Bind e) f = runExists (\(FreerBindF fa k) -> createBind fa (k >=> f) ) e


createBind :: forall f a b. (f b) -> (b -> (Freer f a)) -> Freer f a
createBind m fn = Bind $ mkExists $ FreerBindF m fn

send :: forall f a. f a -> Freer f a
send t = createBind t (\a -> Pure a)
