module Aff.MyAff where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried as Fn
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)

foreign import data Aff :: Type -> Type

instance functorAff :: Functor Aff where
  map = _map

instance applyAff :: Apply Aff where
  apply = ap

instance applicativeAff :: Applicative Aff where
  pure = _pure

instance bindAff :: Bind Aff where
  bind = _bind

instance monadAff :: Monad Aff

instance monadEffectAff :: MonadEffect Aff where
  liftEffect = _liftEffect

newtype Fiber = Fiber { run :: Effect Unit }

launchAff_ :: Aff Unit -> Effect Unit
launchAff_ aff = do
  fiber <- makeFiber aff
  case fiber of Fiber f -> f.run
  pure unit

forkAff :: Aff Unit -> Aff Unit
forkAff = void <<< _fork unit

delay :: Milliseconds -> Aff Unit
delay (Milliseconds n) = Fn.runFn2 _delay Right n

foreign import _pure :: forall a. a -> Aff a
foreign import _fork :: forall a. Unit -> Aff a -> Aff Fiber
foreign import _map :: forall a b. (a -> b) -> Aff a -> Aff b
foreign import _bind :: forall a b. Aff a -> (a -> Aff b) -> Aff b
foreign import _delay :: forall a. Fn.Fn2 (Unit -> Either a Unit) Number (Aff Unit)
foreign import _liftEffect :: forall a. Effect a -> Aff a
foreign import _makeFiber :: forall a. Fn.Fn1 (Aff a) (Effect Fiber)

makeFiber :: Aff Unit -> Effect Fiber
makeFiber aff = Fn.runFn1 _makeFiber aff
