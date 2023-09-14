module Aff.MyAff where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Lazy (class Lazy)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, catchError, try)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Global (Global)
import Control.Plus (class Plus)
import Data.Either (Either(..))
import Data.Function.Uncurried as Fn
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)

-- | An `Aff a` is an asynchronous computation with effects. The
-- | computation may either error with an exception, or produce a result of
-- | type `a`. `Aff` effects are assembled from primitive `Effect` effects using
-- | `makeAff` or `liftEffect`.
foreign import data Aff :: Type -> Type

type role Aff representational

instance functorAff :: Functor Aff where
  map = _map

instance applyAff :: Apply Aff where
  apply = ap

instance applicativeAff :: Applicative Aff where
  pure = _pure

instance bindAff :: Bind Aff where
  bind = _bind

instance monadAff :: Monad Aff

instance semigroupAff :: Semigroup a => Semigroup (Aff a) where
  append = lift2 append

instance monoidAff :: Monoid a => Monoid (Aff a) where
  mempty = pure mempty

instance altAff :: Alt Aff where
  alt a1 a2 = catchError a1 (const a2)

instance plusAff :: Plus Aff where
  empty = throwError (error "Always fails")

-- | This instance is provided for compatibility. `Aff` is always stack-safe
-- | within a given fiber. This instance will just result in unnecessary
-- | bind overhead.
instance monadRecAff :: MonadRec Aff where
  tailRecM k = go
    where
    go a = do
      res <- k a
      case res of
        Done r -> pure r
        Loop b -> go b

instance monadThrowAff :: MonadThrow Error Aff where
  throwError = _throwError

instance monadErrorAff :: MonadError Error Aff where
  catchError = _catchError

instance monadEffectAff :: MonadEffect Aff where
  liftEffect = _liftEffect

instance lazyAff :: Lazy (Aff a) where
  defer f = pure unit >>= f

instance monadSTAff :: MonadST Global Aff where
  liftST = liftST >>> liftEffect


type OnComplete a =
  { rethrow :: Boolean
  , handler :: (Either Error a -> Effect Unit) -> Effect Unit
  }

-- | Represents a forked computation by way of `forkAff`. `Fiber`s are
-- | memoized, so their results are only computed once.
newtype Fiber a = Fiber
  { run :: Effect Unit
  , join :: (Either Error a -> Effect Unit) -> Effect (Effect Unit)
  , onComplete :: OnComplete a -> Effect (Effect Unit)
  , isSuspended :: Effect Boolean
  }

instance functorFiber :: Functor Fiber where
  map f t = unsafePerformEffect (makeFiber (f <$> joinFiber t))

instance applyFiber :: Apply Fiber where
  apply t1 t2 = unsafePerformEffect (makeFiber (joinFiber t1 <*> joinFiber t2))

instance applicativeFiber :: Applicative Fiber where
  pure a = unsafePerformEffect (makeFiber (pure a))

-- | Blocks until the fiber completes, yielding the result. If the fiber
-- | throws an exception, it is rethrown in the current fiber.
joinFiber :: Fiber ~> Aff
joinFiber (Fiber t) = makeAff \k -> effectCanceler <$> t.join k

-- | A cancellation effect for actions run via `makeAff`. If a `Fiber` is
-- | killed, and an async action is pending, the canceler will be called to
-- | clean it up.
newtype Canceler = Canceler (Error -> Aff Unit)

derive instance newtypeCanceler :: Newtype Canceler _


-- | A canceler which does not cancel anything.
nonCanceler :: Canceler
nonCanceler = Canceler (const (pure unit))

-- | A canceler from an Effect action.
effectCanceler :: Effect Unit -> Canceler
effectCanceler = Canceler <<< const <<< liftEffect

-- | Forks an `Aff` from an `Effect` context, returning the `Fiber`.
launchAff :: forall a. Aff a -> Effect (Fiber a)
launchAff aff = do
  fiber <- makeFiber aff
  case fiber of Fiber f -> f.run
  pure fiber

-- | Forks an `Aff` from an `Effect` context, discarding the `Fiber`.
launchAff_ :: Aff Unit -> Effect Unit
launchAff_ = void <<< launchAff

-- | Suspends an `Aff` from an `Effect` context, returning the `Fiber`.
launchSuspendedAff :: forall a. Aff a -> Effect (Fiber a)
launchSuspendedAff = makeFiber

-- | Forks an `Aff` from an `Effect` context and also takes a callback to run when
-- | it completes. Returns the pending `Fiber`.
runAff :: forall a. (Either Error a -> Effect Unit) -> Aff a -> Effect (Fiber Unit)
runAff k aff = launchAff $ liftEffect <<< k =<< try aff

-- | Forks an `Aff` from an `Effect` context and also takes a callback to run when
-- | it completes, discarding the `Fiber`.
runAff_ :: forall a. (Either Error a -> Effect Unit) -> Aff a -> Effect Unit
runAff_ k aff = void $ runAff k aff

-- | Suspends an `Aff` from an `Effect` context and also takes a callback to run
-- | when it completes. Returns the suspended `Fiber`.
runSuspendedAff :: forall a. (Either Error a -> Effect Unit) -> Aff a -> Effect (Fiber Unit)
runSuspendedAff k aff = launchSuspendedAff $ liftEffect <<< k =<< try aff

-- | Forks an `Aff` from within a parent `Aff` context, returning the `Fiber`.
forkAff :: forall a. Aff a -> Aff (Fiber a)
forkAff = _fork true

-- | Suspends an `Aff` from within a parent `Aff` context, returning the `Fiber`.
-- | A suspended `Aff` is not executed until a consumer observes the result
-- | with `joinFiber`.
suspendAff :: forall a. Aff a -> Aff (Fiber a)
suspendAff = _fork false

-- | Pauses the running fiber.
delay :: Milliseconds -> Aff Unit
delay (Milliseconds n) = Fn.runFn2 _delay Right n

-- | A monomorphic version of `try`. Catches thrown errors and lifts them
-- | into an `Either`.
attempt :: forall a. Aff a -> Aff (Either Error a)
attempt = try

-- | Ignores any errors.
apathize :: forall a. Aff a -> Aff Unit
apathize = attempt >>> map (const unit)

foreign import _pure :: forall a. a -> Aff a
foreign import _throwError :: forall a. Error -> Aff a
foreign import _catchError :: forall a. Aff a -> (Error -> Aff a) -> Aff a
foreign import _fork :: forall a. Boolean -> Aff a -> Aff (Fiber a)
foreign import _map :: forall a b. (a -> b) -> Aff a -> Aff b
foreign import _bind :: forall a b. Aff a -> (a -> Aff b) -> Aff b
foreign import _delay :: forall a. Fn.Fn2 (Unit -> Either a Unit) Number (Aff Unit)
foreign import _liftEffect :: forall a. Effect a -> Aff a
foreign import _makeFiber :: forall a. Fn.Fn2 FFIUtil (Aff a) (Effect (Fiber a))


-- | Constructs an `Aff` from low-level `Effect` effects using a callback. A
-- | `Canceler` effect should be returned to cancel the pending action. The
-- | supplied callback may be invoked only once. Subsequent invocation are
-- | ignored.
foreign import makeAff :: forall a. ((Either Error a -> Effect Unit) -> Effect Canceler) -> Aff a

makeFiber :: forall a. Aff a -> Effect (Fiber a)
makeFiber aff = Fn.runFn2 _makeFiber ffiUtil aff

newtype FFIUtil = FFIUtil
  { isLeft :: forall a b. Either a b -> Boolean
  , fromLeft :: forall a b. Either a b -> a
  , fromRight :: forall a b. Either a b -> b
  , left :: forall a b. a -> Either a b
  , right :: forall a b. b -> Either a b
  }

ffiUtil :: FFIUtil
ffiUtil = FFIUtil
  { isLeft
  , fromLeft: unsafeFromLeft
  , fromRight: unsafeFromRight
  , left: Left
  , right: Right
  }
  where
  isLeft :: forall a b. Either a b -> Boolean
  isLeft = case _ of
    Left _ -> true
    Right _ -> false

  unsafeFromLeft :: forall a b. Either a b -> a
  unsafeFromLeft = case _ of
    Left a -> a
    Right _ -> unsafeCrashWith "unsafeFromLeft: Right"

  unsafeFromRight :: forall a b. Either a b -> b
  unsafeFromRight = case _ of
    Right a -> a
    Left _ -> unsafeCrashWith "unsafeFromRight: Left"
