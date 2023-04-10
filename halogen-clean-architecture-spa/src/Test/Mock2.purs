module Test.Mock2
  (
  Cons(..),
  (:),
  calledWith,
  returns,
  returning,
  Definition,
  class MockBuilder,
  mock,
  verify,
  verifyCount,
  calledWithReturns,
  (:>),
  Verifier,
  showCalledArgs
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

infixr 8 type Cons as :
infixr 8 Cons as :

data Definition a r = Definition a r

type Message = String

data VerifyFailed = VerifyFailed Message

instance verifyFailedSemigroup :: Semigroup VerifyFailed where
  append (VerifyFailed m1) (VerifyFailed m2) = VerifyFailed (m1 <> m2)

instance verifyFailedMonoid :: Monoid VerifyFailed where
  mempty = VerifyFailed ""

calledWith :: forall a. a -> Definition a Unit
calledWith a = Definition a unit

returns :: forall a r. Definition a Unit -> r -> Definition a r
returns (Definition a _) r = Definition a r

returning :: forall a r. r ->  Definition a Unit -> Definition a r
returning r (Definition a _) = Definition a r

calledWithReturns :: forall a r. a -> r -> Definition a r
calledWithReturns = calledWith >>> returns

infixr 9 calledWithReturns as :>

class MockBuilder a r v | a -> r, a -> v where
  mock :: a -> Mock r v

instance instanceMockArrayArgs9 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i) 
  => MockBuilder (Array (Definition (a : b : c : d : e : f : g : h : i) r)) (a -> b -> c -> d -> e -> f -> g -> h -> i -> r) (a : b : c : d : e : f : g : h : i) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> storeCalledArgs s (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2 : i2) # findMatchedReturnValue defs),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2 : i2) -> _verify list (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2 : i2)) }
else
instance instanceMockArrayArgs8 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h) 
  => MockBuilder (Array (Definition (a : b : c : d : e : f : g : h) r)) (a -> b -> c -> d -> e -> f -> g -> h -> r) (a : b : c : d : e : f : g : h) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 e2 f2 g2 h2 -> storeCalledArgs s (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2) # findMatchedReturnValue defs),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2) -> _verify list (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2)) }
else
instance instanceMockArrayArgs7 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g) 
  => MockBuilder (Array (Definition (a : b : c : d : e : f : g) r)) (a -> b -> c -> d -> e -> f -> g -> r) (a : b : c : d : e : f : g) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 e2 f2 g2 -> storeCalledArgs s (a2 : b2 : c2 : d2 : e2 : f2 : g2) # findMatchedReturnValue defs),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2 : e2 : f2 : g2) -> _verify list (a2 : b2 : c2 : d2 : e2 : f2 : g2)) }
else
instance instanceMockArrayArgs6 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f) 
  => MockBuilder (Array (Definition (a : b : c : d : e : f) r)) (a -> b -> c -> d -> e -> f -> r) (a : b : c : d : e : f) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 e2 f2 -> storeCalledArgs s (a2 : b2 : c2 : d2 : e2 : f2) # findMatchedReturnValue defs),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2 : e2 : f2) -> _verify list (a2 : b2 : c2 : d2 : e2 : f2)) }
else
instance instanceMockArrayArgs5 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e) 
  => MockBuilder (Array (Definition (a : b : c : d : e) r)) (a -> b -> c -> d -> e -> r) (a : b : c : d : e) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 e2 -> storeCalledArgs s (a2 : b2 : c2 : d2 : e2) # findMatchedReturnValue defs),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2 : e2) -> _verify list (a2 : b2 : c2 : d2 : e2)) }
else
instance instanceMockArrayArgs4 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d) 
  => MockBuilder (Array (Definition (a : b : c : d) r)) (a -> b -> c -> d -> r) (a : b : c : d) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 -> storeCalledArgs s (a2 : b2 : c2 : d2) # findMatchedReturnValue defs),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2) -> _verify list (a2 : b2 : c2 : d2)) }
else
instance instanceMockArrayArgs3 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c) 
  => MockBuilder (Array (Definition (a : b : c) r)) (a -> b -> c -> r) (a : b : c) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 c2 -> storeCalledArgs s (a2 : b2 : c2) # findMatchedReturnValue defs),
      verifier: verifier s.argsList (\list (a2 : b2 : c2) -> _verify list (a2 : b2 : c2)) }
else
instance instanceMockArrayArgs2 :: (Show a, Eq a, Show b, Eq b) => MockBuilder (Array (Definition (a : b) r)) (a -> b -> r) (a : b) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 -> storeCalledArgs s (a2 : b2) # findMatchedReturnValue defs),
      verifier: verifier s.argsList (\list (a2 : b2) -> _verify list (a2 : b2)) }
else
instance instanceMockArrayArgs1 :: (Show a, Eq a) => MockBuilder (Array (Definition a r)) (a -> r) a where
  mock defs = do
    let s = store unit
    { fun: (\a2 -> storeCalledArgs s a2 # findMatchedReturnValue defs),
      verifier: verifier s.argsList (\list a2 -> _verify list a2) }
else
instance instanceMockArgs9 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i) 
  => MockBuilder (Definition (a : b : c : d : e : f : g : h : i) r) (a -> b -> c -> d -> e -> f -> g -> h -> i -> r) (a : b : c : d : e : f : g : h : i) where
  mock (Definition (a : b : c : d : e : f : g : h : i) r) = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> r `const` validateWithStoreArgs s (a : b : c : d : e : f : g : h : i) (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2 : i2)),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2 : i2) -> _verify list (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2 : i2)) }
else
instance instanceMockArgs8 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h) 
  => MockBuilder (Definition (a : b : c : d : e : f : g : h) r) (a -> b -> c -> d -> e -> f -> g -> h -> r) (a : b : c : d : e : f : g : h) where
  mock (Definition (a : b : c : d : e : f : g : h) r) = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 e2 f2 g2 h2 -> r `const` validateWithStoreArgs s (a : b : c : d : e : f : g : h) (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2)),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2) -> _verify list (a2 : b2 : c2 : d2 : e2 : f2 : g2 : h2)) }
else
instance instanceMockArgs7 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g) 
  => MockBuilder (Definition (a : b : c : d : e : f : g) r) (a -> b -> c -> d -> e -> f -> g -> r) (a : b : c : d : e : f : g) where
  mock (Definition (a : b : c : d : e : f : g) r) = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 e2 f2 g2 -> r `const` validateWithStoreArgs s (a : b : c : d : e : f : g) (a2 : b2 : c2 : d2 : e2 : f2 : g2)),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2 : e2 : f2 : g2) -> _verify list (a2 : b2 : c2 : d2 : e2 : f2 : g2)) }
else
instance instanceMockArgs6 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f) 
  => MockBuilder (Definition (a : b : c : d : e : f) r) (a -> b -> c -> d -> e -> f -> r) (a : b : c : d : e : f) where
  mock (Definition (a : b : c : d : e : f) r) = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 e2 f2 -> r `const` validateWithStoreArgs s (a : b : c : d : e : f) (a2 : b2 : c2 : d2 : e2 : f2)),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2 : e2 : f2) -> _verify list (a2 : b2 : c2 : d2 : e2 : f2)) }
else
instance instanceMockArgs5 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e) 
  => MockBuilder (Definition (a : b : c : d : e) r) (a -> b -> c -> d -> e -> r) (a : b : c : d : e) where
  mock (Definition (a : b : c : d : e) r) = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 e2 -> r `const` validateWithStoreArgs s (a : b : c : d : e) (a2 : b2 : c2 : d2 : e2)),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2 : e2) -> _verify list (a2 : b2 : c2 : d2 : e2)) }
else
instance instanceMockArgs4 :: 
     (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d) 
  => MockBuilder (Definition (a : b : c : d) r) (a -> b -> c -> d -> r) (a : b : c : d) where
  mock (Definition (a : b : c : d) r) = do
    let s = store unit
    { fun: (\a2 b2 c2 d2 -> r `const` validateWithStoreArgs s (a : b : c : d) (a2 : b2 : c2 : d2)),
      verifier: verifier s.argsList (\list (a2 : b2 : c2 : d2) -> _verify list (a2 : b2 : c2 : d2)) }
else
instance instanceMockArgs3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c) => MockBuilder (Definition (a : b : c) r) (a -> b -> c -> r) (a : b : c) where
  mock (Definition (a : b : c) r) = do
    let s = store unit
    { fun: (\a2 b2 c2 -> r `const` validateWithStoreArgs s (a : b : c) (a2 : b2 : c2)),
      verifier: verifier s.argsList (\list (a2 : b2 : c2) -> _verify list (a2 : b2 : c2)) }
else
instance instanceMockArgs2 :: (Show a, Eq a, Show b, Eq b) => MockBuilder (Definition (a : b) r) (a -> b -> r) (a : b) where
  mock (Definition (a : b) r) = do
    let s = store unit
    { fun: (\a2 b2 -> r `const` validateWithStoreArgs s (a : b) (a2 : b2)),
      verifier: verifier s.argsList (\list (a2 : b2) -> _verify list (a2 : b2)) }
else
instance instanceMockArgs1 :: (Show a, Eq a) => MockBuilder (Definition a r) (a -> r) a where
  mock (Definition a r) = do
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

findMatchedReturnValue :: forall a r. Eq a => Array (Definition a r) -> a ->  r
findMatchedReturnValue defs args = 
  case find (\(Definition a _) -> a == args) defs of
    Just (Definition _ r) -> r
    Nothing -> error "no answer found."