module Util where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Effect (Effect)
import Type.Equality (class TypeEquals, to)


class X g r c | g->r, r->g where
  x :: c -> g -> r

instance x3 :: X (i -> (a1 -> a2 -> a3 -> o)) (a1 -> a2 -> a3 -> ret) ((i -> o) -> ret) where
  x constructor getter = \a1 a2 a3 -> do
    let i2o = \i -> getter i a1 a2 a3
    constructor i2o
else
instance x2 :: X (i -> (a1 -> a2 -> o)) (a1 -> a2 -> ret) ((i -> o) -> ret) where
  x constructor getter = \a1 a2 -> do
    let i2o = \i -> getter i a1 a2
    constructor i2o
else
instance x1 :: X (i -> (a1 -> o)) (a1 -> ret) ((i -> o) -> ret) where
  x constructor getter = \a1 -> do
    let i2o = \i -> getter i a1
    constructor i2o

f1 :: String -> Effect Unit
f1 _ = pure unit
f2 :: String -> String -> Effect Unit
f2 _ _ = pure unit
f3 :: String -> String -> String -> Effect Unit
f3 _ _ _ = pure unit

r1 :: String -> ReaderT Int Effect Unit
r1 = x ReaderT (\_ -> f1)
r2 :: String -> String -> ReaderT Int Effect Unit
r2 = x ReaderT (\_ -> f2)
r3 :: String -> String -> String -> ReaderT Int Effect Unit
r3 = x ReaderT (\_ -> f3)


class C input output | input -> output, output -> input where
  reader2 :: input -> output

instance c3 :: TypeEquals r x => C (x -> a1 -> a2 -> a3 -> m a) (a1 -> a2 -> a3 -> ReaderT r m a) where
  reader2 f = x ReaderT (to >>> f $ _)
else
instance c2 :: TypeEquals r x => C (x -> a1 -> a2 -> m a) (a1 -> a2 -> ReaderT r m a) where
  reader2 f = x ReaderT (to >>> f $ _)
else
instance c1 :: TypeEquals r x => C (x -> a1 -> m a) (a1 -> ReaderT r m a) where
  reader2 f = x ReaderT (to >>> f $ _)


class Monad m <= T m where
  a1 :: String -> m Unit
  a2 :: String -> String -> m Unit
  a3 :: String -> String -> String -> m Unit

type TFunction m = {
  a1 :: String -> m Unit,
  a2 :: String -> String -> m Unit,
  a3 :: String -> String -> String -> m Unit
}

instance instanceT 
  :: (Monad m, TypeEquals f (TFunction m))
  => T (ReaderT f m) where
  a1 = reader2 _.a1
  a2 = reader2 _.a2
  a3 = reader2 _.a3


