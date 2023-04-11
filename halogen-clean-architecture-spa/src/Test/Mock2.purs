module Test.Mock2
  (
  Cons(..),
  Definition,
  class MockBuilder,
  mock,
  verify,
  verifyCount,
  (:>),
  Verifier,
  showCalledArgs,
  whenCalledWith,
  thenReturn,
  returning
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (any, filter, find, length)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect.Exception (Error, throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.Spec.Assertions (fail)

type Mock fn v = {
  fun :: fn,
  verifier :: Verifier v
}

foreign import store :: forall a. Unit -> CallredArgsStore a

type CallredArgsStore v = {
  argsList :: CalledArgsList v,
  store :: v -> Unit
}

-- CalledArgs (a : b : c)
type CalledArgsList v = Array v

newtype Verifier v = Verifier {
  calledArgsList :: CalledArgsList v,
  verify :: CalledArgsList v -> v -> Maybe VerifyFailed
}

verifier :: forall v. CalledArgsList v -> (CalledArgsList v -> v -> Maybe VerifyFailed) -> Verifier v
verifier l f = Verifier { calledArgsList: l, verify: f }

data Cons a b = Cons a b

instance showCons :: (Show a, Show b) => Show (Cons a b) where
  show (Cons a b) = (show a) <> ", " <> (show b)

instance eqCons :: (Eq a, Eq b) => Eq (Cons a b) where
  eq (Cons a b) (Cons a2 b2) = (eq a a2) && (eq b b2)

infixr 8 type Cons as :>
infixr 8 Cons as :>

data Definition a = Definition a

type Message = String

data VerifyFailed = VerifyFailed Message

instance verifyFailedSemigroup :: Semigroup VerifyFailed where
  append (VerifyFailed m1) (VerifyFailed m2) = VerifyFailed (m1 <> m2)

instance verifyFailedMonoid :: Monoid VerifyFailed where
  mempty = VerifyFailed ""

class MockBuilder a r v | a -> r, a -> v where
  mock :: a -> Mock r v

class Finder d a r | d -> a, d -> r where
  findMatchedReturnValue :: Array d -> a ->  r

instance instanceMockArrayArgs3 :: (Show a, Eq a, Show b, Eq b) => MockBuilder (Array (a :> b :> r)) (a -> b -> r) (a :> b) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 -> (case find (\(a :> b :> _) -> (a :> b) == (a2 :> b2)) defs of
                      Just (_ :> _ :> r) -> r
                      Nothing -> error "no answer found.") `const` storeCalledArgs s (a2 :> b2)),
      verifier: verifier s.argsList (\list (a2 :> b2) -> _verify list (a2 :> b2)) }
else
instance instanceMockArrayArgs2 :: (Show a, Eq a) => MockBuilder (Array (a :> r)) (a -> r) a where
  mock defs = do
    let s = store unit
    { fun: (\a2 -> (case find (\(a :> _) -> a == a2) defs of
                      Just (_ :> r) -> r
                      Nothing -> error "no answer found.") `const` storeCalledArgs s a2),
      verifier: verifier s.argsList (\list a2 -> _verify list a2) }
else
instance instanceMockArgs4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c) => MockBuilder (a :> b :> c :> r) (a -> b -> c -> r) (a :> b :> c) where
  mock (a :> b :> c :> r) = do
    let s = store unit
    { fun: (\a2 b2 c2 -> r `const` validateWithStoreArgs s (a :> b :> c) (a2 :> b2 :> c2)),
      verifier: verifier s.argsList (\list (a2 :> b2 :> c2) -> _verify list (a2 :> b2 :> c2)) }
else
instance instanceMockArgs3 :: (Show a, Eq a, Show b, Eq b) => MockBuilder (a :> b :> r) (a -> b -> r) (a :> b) where
  mock (a :> b :> r) = do
    let s = store unit
    { fun: (\a2 b2 -> r `const` validateWithStoreArgs s (a :> b) (a2 :> b2)),
      verifier: verifier s.argsList (\list (a2 :> b2) -> _verify list (a2 :> b2)) }
else
instance instanceMockArgs2 :: (Show a, Eq a) => MockBuilder (a :> r) (a -> r) a where
  mock (a :> r) = do
    let s = store unit
    { fun: (\a2 -> r `const` validateWithStoreArgs s a a2),
      verifier: verifier s.argsList (\list a2 -> _verify list a2) }


_verify :: forall a. Eq a => Show a => Array a -> a -> Maybe VerifyFailed
_verify list a = 
  if any (_ == a) list then Nothing 
  else Just $ VerifyFailed ("No answer found for argument: " <> show a)

verify :: forall a m r. MonadThrow Error m => { verifier :: Verifier a | r} -> a -> m Unit
verify {verifier : (Verifier { calledArgsList, verify: doVerify })} args = 
  case doVerify calledArgsList args of
    Just (VerifyFailed msg) -> fail msg
    Nothing -> pure unit

validateArgs :: forall a. Eq a => Show a => a -> a -> Unit
validateArgs expected actual = if (expected == actual) then unit else error $ message expected actual

storeCalledArgs :: forall a. CallredArgsStore a -> a -> a
storeCalledArgs s a = const a (s.store a)

validateWithStoreArgs :: forall a. Eq a => Show a => CallredArgsStore a -> a -> a -> Unit
validateWithStoreArgs s expected actual = validateArgs expected (storeCalledArgs s actual)

verifyCount :: forall v m r. MonadThrow Error m => Eq v => { verifier :: Verifier v | r} -> v -> Int -> m Unit
verifyCount {verifier : (Verifier { calledArgsList })} v count =
  let
    callCount = length (filter (\args -> args == v) calledArgsList)
  in if count == callCount then pure unit
    else fail $ joinWith "\n" ["Function was not called the expected number of times.",  "expected: " <> show count, "but was : " <> show callCount]


showCalledArgs :: forall v r. Eq v => Show v => { verifier :: Verifier v | r} -> v -> String
showCalledArgs { verifier : (Verifier { calledArgsList }) } v = show (filter (\args -> args == v) calledArgsList)

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: forall a. Show a => a -> a -> String
message expected actual = joinWith "\n" ["Function was not called with expected arguments.",  "expected: " <> show expected, "but was : " <> show actual]

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

whenCalledWith :: forall a. a -> a
whenCalledWith = identity

returning :: forall a b. a -> b -> Cons b a
returning a b = b :> a 

thenReturn :: forall a b. a -> b -> Cons a b
thenReturn = (:>)