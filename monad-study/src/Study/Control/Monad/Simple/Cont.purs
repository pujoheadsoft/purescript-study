module Study.Control.Monad.Simple.Cont where

import Prelude

newtype Cont r a = Cont ((a -> r) -> r)

runCont :: forall r a. Cont r a -> (a -> r) -> r
runCont (Cont f) k = f k

instance functorCont :: Functor (Cont r) where
  map :: forall a b. (a -> b) -> (Cont r a) -> (Cont r b)
  map f (Cont c) = Cont (\(k :: (b -> r)) -> c (\a -> k $ f a))

instance applyCont :: Apply (Cont r) where
  apply :: forall a b. (Cont r (a -> b)) -> (Cont r a) -> (Cont r b)
  apply
    (Cont (f :: (((a -> b) -> r) -> r)))
    (Cont (v :: ((a -> r) -> r))) 
    = Cont (\(k :: (b -> r)) -> f (\(g :: (a -> b)) -> v (\a -> k (g a))))

instance applicativeCont :: Applicative (Cont r) where
  pure a = Cont (\k -> k a)

instance bindCont :: Bind (Cont r) where
  bind :: forall a b. Cont r a -> (a -> (Cont r b)) -> Cont r b
  bind (Cont c) f = Cont 
    (\(k :: (b -> r)) -> 
      c (\a -> case f a of 
        (Cont (x :: ((b -> r) -> r))) -> x k))

instance monadCont :: Monad (Cont r)