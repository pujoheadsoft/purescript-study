module Test.Mock3
  (
  Cons(..),
  class MockBuilder,
  mock,
  verify,
  class VerifyCountBuilder,
  verifyCount,
  (#>),
  (:>),
  cons,
  class ConsGen,
  class VerifyBuilder,
  Verifier,
  showCalledParams,
  Param,
  param,
  any,
  anyV,
  matcher,
  Mock,
  runRuntimeThrowableFunction,
  CountVerifyMethod(..)
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (filter, find, length)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect.Exception (Error, throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.Spec.Assertions (fail)
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

type Mock fn v = {
  fun :: fn,
  verifier :: Verifier v
}

foreign import store :: forall a. Unit -> CallredParamsStore a

type CallredParamsStore v = {
  argsList :: CalledParamsList v,
  store :: v -> Unit
}

type CalledParamsList v = Array v

newtype Verifier v = Verifier {
  calledParamsList :: CalledParamsList v,
  verify :: CalledParamsList v -> v -> Maybe VerifyFailed
}

verifier :: forall v. CalledParamsList v -> (CalledParamsList v -> v -> Maybe VerifyFailed) -> Verifier v
verifier l f = Verifier { calledParamsList: l, verify: f }

data Cons a b = Cons a b

instance showCons :: (Show a, Show b) => Show (Cons a b) where
  show (Cons a b) = (show a) <> ", " <> (show b)

instance eqCons :: (Eq a, Eq b) => Eq (Cons a b) where
  eq (Cons a b) (Cons a2 b2) = (eq a a2) && (eq b b2)

infixr 8 type Cons as #>
infixr 8 Cons as #>

type Message = String

data VerifyFailed = VerifyFailed Message

instance verifyFailedSemigroup :: Semigroup VerifyFailed where
  append (VerifyFailed m1) (VerifyFailed m2) = VerifyFailed (m1 <> m2)

instance verifyFailedMonoid :: Monoid VerifyFailed where
  mempty = VerifyFailed ""

class MockBuilder a r v | a -> r, a -> v where
  mock :: a -> Mock r v

class Finder d a r | d -> a, d -> r where
  findMatchedReturnValue :: Array d -> a -> Maybe r

instance finderArg9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => 
  Finder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i)
    r where
  findMatchedReturnValue defs args = do
    find (\(a #> b #> c #> d #> e #> f #> g #> h #> i #> _) -> (a #> b #> c #> d #> e #> f #> g #> h #> i) == args) defs
      >>= \(_ #> _ #> _ #> _ #> _ #> _ #> _ #> _ #> _ #> r) -> pure $ value r
else
instance finderArg8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => 
  Finder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h)
    r where
  findMatchedReturnValue defs args = do
    find (\(a #> b #> c #> d #> e #> f #> g #> h #> _) -> (a #> b #> c #> d #> e #> f #> g #> h) == args) defs
      >>= \(_ #> _ #> _ #> _ #> _ #> _ #> _ #> _ #> r) -> pure $ value r
else
instance finderArg7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => 
  Finder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g)
    r where
  findMatchedReturnValue defs args = do
    find (\(a #> b #> c #> d #> e #> f #> g #> _) -> (a #> b #> c #> d #> e #> f #> g) == args) defs
      >>= \(_ #> _ #> _ #> _ #> _ #> _ #> _ #> r) -> pure $ value r
else
instance finderArg6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => 
  Finder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f)
    r where
  findMatchedReturnValue defs args = do
    find (\(a #> b #> c #> d #> e #> f #> _) -> (a #> b #> c #> d #> e #> f) == args) defs
      >>= \(_ #> _ #> _ #> _ #> _ #> _ #> r) -> pure $ value r
else
instance finderArg5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) => 
  Finder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param r)
    (Param a #> Param b #> Param c #> Param d #> Param e)
    r where
  findMatchedReturnValue defs args = do
    find (\(a #> b #> c #> d #> e #> _) -> (a #> b #> c #> d #> e) == args) defs
      >>= \(_ #> _ #> _ #> _ #> _ #> r) -> pure $ value r
else
instance finderArg4 :: (Eq a, Eq b, Eq c, Eq d) => 
  Finder
    (Param a #> Param b #> Param c #> Param d #> Param r)
    (Param a #> Param b #> Param c #> Param d)
    r where
  findMatchedReturnValue defs args = do
    find (\(a #> b #> c #> d #> _) -> (a #> b #> c #> d) == args) defs
      >>= \(_ #> _ #> _ #> _ #> r) -> pure $ value r
else
instance finderArg3 :: (Eq a, Eq b, Eq c) => 
  Finder
    (Param a #> Param b #> Param c #> Param r)
    (Param a #> Param b #> Param c)
    r where
  findMatchedReturnValue defs args = do
    find (\(a #> b #> c #> _) -> (a #> b #> c) == args) defs
      >>= \(_ #> _ #> _ #> r) -> pure $ value r
else
instance finderArg2 :: (Eq a, Eq b) => Finder (Param a #> Param b #> Param r) (Param a #> Param b) r where
  findMatchedReturnValue defs args = do
    find (\(a #> b #> _) -> (a #> b) == args) defs
      >>= \(_ #> _ #> r) -> pure $ value r
else
instance finderArg1 :: Eq a => Finder (Param a #> Param r) (Param a) r where
  findMatchedReturnValue defs a2 = do
    find (\(a #> _) -> a == a2) defs
      >>= \(_ #> r) -> pure $ value r

_findMatchedReturnValue :: forall r a v. Finder v a r => CalledParamsList v -> a -> CallredParamsStore a -> r
_findMatchedReturnValue defs a s =
  let
    r = findMatchedReturnValue defs a
    _ = storeCalledParams s a
  in case r of
    Just v -> v
    Nothing -> error "no answer found."

instance instanceMockArrayArg9 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> _findMatchedReturnValue defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) s)
else
instance instanceMockArrayArg8 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 h2 -> _findMatchedReturnValue defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) s)
else
instance instanceMockArrayArg7 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r))
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 -> _findMatchedReturnValue defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) s)
else
instance instanceMockArrayArg6 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r))
    (a -> b -> c -> d -> e -> f -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 f2 -> _findMatchedReturnValue defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) s)
else
instance instanceMockArrayArg5 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param e #> Param r))
    (a -> b -> c -> d -> e -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 e2 -> _findMatchedReturnValue defs (p a2 :> p b2 :> p c2 :> p d2 :> p e2) s)
else
instance instanceMockArrayArg4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param d #> Param r))
    (a -> b -> c -> d -> r)
    (Param a #> Param b #> Param c #> Param d) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 d2 -> _findMatchedReturnValue defs (p a2 :> p b2 :> p c2 :> p d2) s)
else
instance instanceMockArrayArg3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder 
    (Array (Param a #> Param b #> Param c #> Param r))
    (a -> b -> c -> r)
    (Param a #> Param b #> Param c) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 c2 -> _findMatchedReturnValue defs (p a2 :> p b2 :> p c2) s)
else
instance instanceMockArrayArg2 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Array (Param a #> Param b #> Param r)) (a -> b -> r) (Param a #> Param b) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 b2 -> _findMatchedReturnValue defs (p a2 :> p b2) s)
else
instance instanceMockArrayArg1 :: (Show a, Eq a)
  => MockBuilder (Array (Param a #> Param r)) (a -> r) (Param a) where
  mock defs = do
    let s = store unit
    mockT s.argsList (\a2 -> _findMatchedReturnValue defs (p a2) s)
else
instance instanceMockArg9 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h, Show i, Eq i)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i) where
  mock defs =
    let s = store unit
    in mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 h2 i2 -> (returnValue defs)
      `const` validateWithStoreParams s 
        (args defs)
        (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2 :> p i2) )
else
instance instanceMockArg8 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g, Show h, Eq h)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> h -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h) where
  mock defs =
    let s = store unit
    in mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 h2 -> (returnValue defs)
      `const` validateWithStoreParams s 
        (args defs)
        (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2 :> p h2) )
else
instance instanceMockArg7 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f, Show g, Eq g)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r)
    (a -> b -> c -> d -> e -> f -> g -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g) where
  mock defs =
    let s = store unit
    in mockT s.argsList (\a2 b2 c2 d2 e2 f2 g2 -> (returnValue defs)
      `const` validateWithStoreParams s 
        (args defs)
        (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2 :> p g2) )
else
instance instanceMockArg6 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e, Show f, Eq f)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r)
    (a -> b -> c -> d -> e -> f -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param f) where
  mock defs =
    let s = store unit
    in mockT s.argsList (\a2 b2 c2 d2 e2 f2 -> (returnValue defs) `const` 
      validateWithStoreParams s (args defs) (p a2 :> p b2 :> p c2 :> p d2 :> p e2 :> p f2) )
else
instance instanceMockArg5 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d, Show e, Eq e)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param e #> Param r)
    (a -> b -> c -> d -> e -> r)
    (Param a #> Param b #> Param c #> Param d #> Param e) where
  mock defs =
    let s = store unit
    in mockT s.argsList (\a2 b2 c2 d2 e2 -> (returnValue defs) `const` 
      validateWithStoreParams s (args defs) (p a2 :> p b2 :> p c2 :> p d2 :> p e2) )
else
instance instanceMockArg4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c, Show d, Eq d)
  => MockBuilder
    (Param a #> Param b #> Param c #> Param d #> Param r)
    (a -> b -> c -> d -> r)
    (Param a #> Param b #> Param c #> Param d) where
  mock defs =
    let s = store unit
    in mockT s.argsList (\a2 b2 c2 d2 -> (returnValue defs) `const` 
      validateWithStoreParams s (args defs) (p a2 :> p b2 :> p c2 :> p d2) )
else
instance instanceMockArg3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder (Param a #> Param b #> Param c #> Param r) (a -> b -> c -> r) (Param a #> Param b #> Param c) where
  mock defs =
    let s = store unit
    in mockT s.argsList (\a2 b2 c2 -> (returnValue defs) `const` validateWithStoreParams s (args defs) (p a2 :> p b2 :> p c2) )
else
instance instanceMockArg2 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Param a #> Param b #> Param r) (a -> b -> r) (Param a #> Param b) where
  mock defs =
    let s = store unit
    in mockT s.argsList (\a2 b2 -> returnValue defs `const` validateWithStoreParams s (args defs) (p a2 :> p b2))
else
instance instanceMockArg1 :: (Show a, Eq a) 
  => MockBuilder (Param a #> Param r) (a -> r) (Param a) where
  mock defs =
    let s = store unit
    in mockT s.argsList (\a2 -> returnValue defs `const` validateWithStoreParams s (args defs) (p a2))

returnValue :: forall defs args r. Extractor defs args (Param r) => defs -> r
returnValue = return >>> value

mockT :: forall fun v. Eq v => Show v => CalledParamsList v -> fun -> Mock fun v
mockT argsList fun = {
  fun,
  verifier: verifier argsList (\list args -> _verify list args)
}

type Matcher v = v -> v -> Boolean

anyMatcher :: forall a. a -> a -> Boolean
anyMatcher _ _ = true

newtype Param v = Param {
  v :: v,
  matcher :: Maybe (Matcher v)
}

value :: forall v. Param v -> v
value (Param {v, matcher: _}) = v

instance eqParam :: Eq a => Eq (Param a) where
  eq (Param {v: a, matcher: (Just m1)}) (Param {v: b, matcher: (Just m2)}) = (m1 a b) && (m2 a b)
  eq (Param {v: a, matcher: (Just m1)}) (Param {v: b, matcher: Nothing})   = m1 a b
  eq (Param {v: a, matcher: Nothing})   (Param {v: b, matcher: (Just m2)}) = m2 a b
  eq (Param {v: a, matcher: Nothing})   (Param {v: b, matcher: Nothing})   = a == b

instance showParam :: Show a => Show (Param a) where
  show (Param {v, matcher: _}) = show v

class ConsGen a b r | a -> r, b -> r where
  cons :: a -> b -> r

instance instaneConsGen9 :: ConsGen (Cons a b) (Cons b c) (Cons (Cons a b) (Cons b c)) where
  cons = Cons
else
instance instaneConsGen8 :: ConsGen (Cons a b) (Param b) (Cons (Cons a b) (Param b)) where
  cons = Cons
else
instance instaneConsGen7 :: ConsGen (Param a) (Cons b c) (Cons (Param a) (Cons b c)) where
  cons = Cons
else
instance instaneConsGen6 :: ConsGen a (Cons b c) (Cons (Param a) (Cons b c)) where
  cons a b = Cons (Param {v: a, matcher: Nothing}) b
else
instance instaneConsGen5 :: ConsGen (Cons a b) c (Cons (Cons a b) (Param c)) where
  cons a b = Cons a (Param {v: b, matcher: Nothing})
else
instance instaneConsGen4 :: ConsGen (Param a) (Param b) (Cons (Param a) (Param b)) where
  cons = Cons
else
instance instaneConsGen3 :: ConsGen a (Param b) (Cons (Param a) (Param b)) where
  cons a b = Cons (Param {v: a, matcher: Nothing}) b
else
instance instaneConsGen2 :: ConsGen (Param a) b (Cons (Param a) (Param b)) where
  cons a b = Cons a (Param {v: b, matcher: Nothing})
else
instance instaneConsGen :: ConsGen a b (Cons (Param a) (Param b)) where
  cons a b = Cons (param a) (param b)

param :: forall a. a -> Param a
param a = Param {v: a, matcher: Nothing}

p = param

infixr 8 cons as :>

_verify :: forall a. Eq a => Show a => Array a -> a -> Maybe VerifyFailed
_verify list a = 
  if A.any (a == _) list then Nothing 
  else Just $ verifyFailedMesssage list a

verifyFailedMesssage :: forall a. Show a => Array a -> a -> VerifyFailed
verifyFailedMesssage calledParams expected
  = VerifyFailed $ joinWith "\n" ["Function was not called with expected arguments.",  "  expected: " <> show expected, "  but was : " <> show calledParams]

class VerifyBuilder v a where
  verify :: forall m r. MonadThrow Error m => { verifier :: Verifier v | r} -> a -> m Unit

instance instanceVerifyBuilderParam1 :: Eq a => VerifyBuilder (Param a) a where
  verify {verifier : (Verifier { calledParamsList, verify: doVerify })} a = 
    zzz doVerify calledParamsList (param a)
else
instance instanceVerifyBuilder :: VerifyBuilder a a where
  verify {verifier : (Verifier { calledParamsList, verify: doVerify })} args = 
    zzz doVerify calledParamsList args

zzz :: forall v m. MonadThrow Error m => (CalledParamsList v -> v -> Maybe VerifyFailed) -> CalledParamsList v -> v -> m Unit
zzz doVerify calledParamsList args = case doVerify calledParamsList args of
      Just (VerifyFailed msg) -> fail msg
      Nothing -> pure unit

validateParams :: forall a. Eq a => Show a => a -> a -> Unit
validateParams expected actual = if (expected == actual) then unit else error $ message expected actual

storeCalledParams :: forall a. CallredParamsStore a -> a -> a
storeCalledParams s a = const a (s.store a)

validateWithStoreParams :: forall a. Eq a => Show a => CallredParamsStore a -> a -> a -> Unit
validateWithStoreParams s expected actual = validateParams expected (storeCalledParams s actual)

class VerifyCountBuilder c v a where
  verifyCount :: forall m r. MonadThrow Error m => Eq v => { verifier :: Verifier v | r} -> c -> a -> m Unit

instance instanceVerifyCountBuilder3 ::  Eq a => VerifyCountBuilder CountVerifyMethod (Param a) a where
  verifyCount {verifier : (Verifier { calledParamsList })} count a = yyy calledParamsList (param a) count
else
instance instanceVerifyCountBuilderParam1 :: Eq a => VerifyCountBuilder Int (Param a) a where
  verifyCount {verifier : (Verifier { calledParamsList })} count a =  yyy calledParamsList (param a) (Equal count)
else
instance instanceVerifyCountBuilder2 :: VerifyCountBuilder CountVerifyMethod a a where
  verifyCount {verifier : (Verifier { calledParamsList })} count a = yyy calledParamsList a count
else
instance instanceVerifyCountBuilder :: VerifyCountBuilder Int a a where
  verifyCount {verifier : (Verifier { calledParamsList })} count a = yyy calledParamsList a (Equal count)

data CountVerifyMethod =
    Equal Int
  | LessThanEqual Int
  | GreaterThanEqual Int
  | LessThan Int
  | GreaterThan Int

compareCount :: CountVerifyMethod -> Int -> Boolean
compareCount (Equal e) a            = a == e
compareCount (LessThanEqual e) a    = a <= e
compareCount (LessThan e) a         = a <  e
compareCount (GreaterThanEqual e) a = a >= e
compareCount (GreaterThan e) a      = a >  e

instance showCountVerifyMethod :: Show CountVerifyMethod where
  show (Equal e)            = show e
  show (LessThanEqual e)    = "<= " <> show e
  show (LessThan e)         = "< " <> show e
  show (GreaterThanEqual e) = ">= " <> show e
  show (GreaterThan e)      = "> " <> show e

yyy :: forall v m. MonadThrow Error m => Eq v => CalledParamsList v -> v -> CountVerifyMethod -> m Unit
yyy calledParamsList v method = 
  let
    callCount = length (filter (\args -> v == args) calledParamsList)
  in if compareCount method callCount then pure unit
    else fail $ joinWith "\n" ["Function was not called the expected number of times.",  "expected: " <> show method, "but was : " <> show callCount]

showCalledParams :: forall v r. Eq v => Show v => { verifier :: Verifier v | r} -> v -> String
showCalledParams { verifier : (Verifier { calledParamsList }) } v = show (filter (\args -> args == v) calledParamsList)

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: forall a. Show a => a -> a -> String
message expected actual = joinWith "\n" ["Function was not called with expected arguments.",  "  expected: " <> show expected, "  but was : " <> show actual]

error :: forall a. String -> a
error = unsafePerformEffect <<< throw


any :: forall a. Param a
any = unsafeCoerce Param {v: "any", matcher: Just anyMatcher}

anyV :: forall a.  a -> Param a
anyV a = Param {v: a, matcher: Just anyMatcher}

matcher :: forall a. (a -> Boolean) -> String -> Param a
matcher f m = Param {v: unsafeCoerce m, matcher: Just (\_ a -> f a)}

cc = any :: Param Int

dd :: Cons (Param String) (Cons (Param Int) (Param Boolean))
dd = param "a" #> param 1 #> param true

dd2 :: Cons (Param String) (Cons (Param Int) (Cons (Param Boolean) (Param String)))
dd2 = param "a" #> param 1 #> param true #> param "b"

ee :: Cons (Param String) (Param Int)
ee = "a" :> 1

ff :: Cons (Param String) (Cons (Param Int) (Param Boolean))
ff = "a" :> 1 :> true

gg :: Cons (Param String) (Cons (Param Int) (Cons (Param Boolean) (Param String)))
gg = "a" :> 1 :> true :> "b"


hh :: Cons (Param String) (Cons (Param Int) (Param Boolean))
hh = anyV "a" :> anyV 1 :> anyV true

type TryCatchResult r = {
  hasError :: Boolean,
  error :: String,
  result :: r
}

foreign import _runRuntimeThrowableFunction :: forall r. (Unit -> r) -> TryCatchResult r

runRuntimeThrowableFunction :: forall r m. MonadThrow Error m => (Unit -> r) -> m Unit
runRuntimeThrowableFunction f =
  let
    r = _runRuntimeThrowableFunction f
  in if r.hasError then fail r.error else pure unit

class Extractor d a r | d -> a, d -> r where
  args :: d -> a
  return :: d -> r

instance extractor9 :: Extractor 
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i #> Param r)
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param i) (Param r) where
  args (a #> b #> c #> d #> e #> f #> g #> h #> i #> _) = a #> b #> c #> d #> e #> f #> g #> h #> i
  return (_ #> _ #> _ #> _ #> _ #> _ #> _ #> _ #> _ #> r) = r
else
instance extractor8 :: Extractor 
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h #> Param r)
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param h) (Param r) where
  args (a #> b #> c #> d #> e #> f #> g #> h #> _) = a #> b #> c #> d #> e #> f #> g #> h
  return (_ #> _ #> _ #> _ #> _ #> _ #> _ #> _ #> r) = r
else
instance extractor7 :: Extractor 
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g #> Param r)
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param g) (Param r) where
  args (a #> b #> c #> d #> e #> f #> g #> _) = a #> b #> c #> d #> e #> f #> g
  return (_ #> _ #> _ #> _ #> _ #> _ #> _ #> r) = r
else
instance extractor6 :: Extractor 
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f #> Param r)
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param f) (Param r) where
  args (a #> b #> c #> d #> e #> f #> _) = a #> b #> c #> d #> e #> f
  return (_ #> _ #> _ #> _ #> _ #> _ #> r) = r
else
instance extractor5 :: Extractor 
  (Param a #> Param b #> Param c #> Param d #> Param e #> Param r)
  (Param a #> Param b #> Param c #> Param d #> Param e) (Param r) where
  args (a #> b #> c #> d #> e #> _) = a #> b #> c #> d #> e
  return (_ #> _ #> _ #> _ #> _ #> r) = r
else
instance extractor4 :: Extractor (Param a #> Param b #> Param c #> Param d #> Param r) (Param a #> Param b #> Param c #> Param d) (Param r) where
  args (a #> b #> c #> d #> _) = a #> b #> c #> d
  return (_ #> _ #> _ #> _ #> r) = r
else
instance extractor3 :: Extractor (Param a #> Param b #> Param c #> Param r) (Param a #> Param b #> Param c) (Param r) where
  args (a #> b #> c #> _) = a #> b #> c
  return (_ #> _ #> _ #> r) = r
else
instance extractor2 :: Extractor (Param a #> Param b #> Param r) (Param a #> Param b) (Param r) where
  args (a #> b #> _) = a #> b
  return (_ #> _ #> r) = r
else
instance extractor1 :: Extractor (Param a #> Param r) (Param a) (Param r) where
  args (a #> _) = a
  return (_ #> r) = r
