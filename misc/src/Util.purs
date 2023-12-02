module Util where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (ReaderT(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Type.Equality (class TypeEquals, to)


class X function return construcor | function -> return, return -> function where
  compose :: construcor -> function -> return

instance x3 :: X (i -> (a1 -> a2 -> a3 -> o)) (a1 -> a2 -> a3 -> ret) ((i -> o) -> ret) where
  compose constructor function a1 a2 a3 = constructor $ \i -> function i a1 a2 a3
else
instance xr3 :: X (a1 -> a2 -> a3 -> o) (a1 -> a2 -> a3 -> m o) (o -> m o) where
  compose constructor function a1 a2 a3 = constructor $ function a1 a2 a3
else
instance x2 :: X (i -> (a1 -> a2 -> o)) (a1 -> a2 -> ret) ((i -> o) -> ret) where
  compose constructor function a1 a2 = constructor $ \i -> function i a1 a2
else
instance xr2 :: X (a1 -> a2 -> o) (a1 -> a2 -> m o) (o -> m o) where
  compose constructor function a1 a2 = constructor $ function a1 a2
else
instance x1 :: X (i -> (a1 -> o)) (a1 -> ret) ((i -> o) -> ret) where
  compose constructor function a1 = constructor $ \i -> function i a1
else
instance xr1 :: X (a1 -> o) (a1 -> m o) (o -> m o) where
  compose constructor function a1 = constructor $ function a1

infixr 10 compose as <<*

m1 :: Int -> Maybe Int
m1 = Just <<* i1
m2 :: Int -> Int -> Maybe Int
m2 = Just <<* i2
m3 :: Int -> Int -> Int -> Maybe Int
m3 = Just <<* i3

mt2 :: Int -> Int -> MaybeT Effect Int
mt2 = pure <<* i2

f1 :: String -> Effect Unit
f1 _ = pure unit
f2 :: String -> String -> Effect Unit
f2 _ _ = pure unit
f3 :: String -> String -> String -> Effect Unit
f3 _ _ _ = pure unit

i1 :: Int -> Int
i1 _ = 1
i2 :: Int -> Int -> Int
i2 _ _ = 10
i3 :: Int -> Int -> Int -> Int
i3 _ _ _ = 100

r1 :: String -> ReaderT Int Effect Unit
r1 = ReaderT <<* (\_ -> f1)
r2 :: String -> String -> ReaderT Int Effect Unit
r2 = ReaderT <<* (\_ -> f2)
r3 :: String -> String -> String -> ReaderT Int Effect Unit
r3 = ReaderT <<* (\_ -> f3)

class C input output | input -> output, output -> input where
  reader2 :: input -> output

instance c3 :: TypeEquals r x => C (x -> a1 -> a2 -> a3 -> m a) (a1 -> a2 -> a3 -> ReaderT r m a) where
  reader2 f = ReaderT <<* (to >>> f $ _)
else
instance c2 :: TypeEquals r x => C (x -> a1 -> a2 -> m a) (a1 -> a2 -> ReaderT r m a) where
  reader2 f = ReaderT <<* (to >>> f $ _)
else
instance c1 :: TypeEquals r x => C (x -> a1 -> m a) (a1 -> ReaderT r m a) where
  reader2 f = ReaderT <<* (to >>> f $ _)


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


