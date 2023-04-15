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
  matcher
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (filter, find, length)
import Data.Array as A
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
  findMatchedReturnValue :: Array d -> a ->  r

instance finderParam3 :: (Eq a, Eq b) => Finder (Param a #> Param b #> Param r) (Param a #> Param b) r where
  findMatchedReturnValue defs (a2 #> b2) = case find (\(a #> b #> _) -> (a #> b) == (a2 #> b2)) defs of
    Just (_ #> _ #> (Param {v: r, matcher: _})) -> r
    Nothing -> error "no answer found."
else
instance finderParam2 :: Eq a => Finder (Param a #> Param r) (Param a) r where
  findMatchedReturnValue defs a2 = case find (\(a #> _) -> a == a2) defs of
    Just (_ #> (Param {v: r, matcher: _})) -> r
    Nothing -> error "no answer found."

instance instanceMockArrayParams3 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Array (Param a #> Param b #> Param r)) (a -> b -> r) (Param a #> Param b) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 -> findMatchedReturnValue defs (param a2 :> param b2) `const` storeCalledParams s (param a2 :> param b2)),
      verifier: verifier s.argsList (\list (a2 #> b2) -> _verify list (a2 #> b2)) }
else
instance instanceMockArrayParams2 :: (Show a, Eq a)
  => MockBuilder (Array (Param a #> Param r)) (a -> r) (Param a) where
  mock defs = do
    let s = store unit
    { fun: (\a2 -> findMatchedReturnValue defs (param a2) `const` storeCalledParams s (param a2)),
      verifier: verifier s.argsList (\list a2 -> _verify list a2) }
else
instance instanceMockParams4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder (Param a #> Param b #> Param c #> Param r) (a -> b -> c -> r) (Param a #> Param b #> Param c) where
  mock (a #> b #> c #> (Param {v: r, matcher: _})) = do
    let s = store unit
    { fun: (\a2 b2 c2 -> r `const` validateWithStoreParams s (a #> b #> c) (param a2 :> param b2 :> param c2) ),
      verifier: verifier s.argsList (\list (a2 #> b2 #> c2) -> _verify list (a2 #> b2 #> c2)) }
else
instance instanceMockParams3 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Param a #> Param b #> Param r) (a -> b -> r) (Param a #> Param b) where
  mock (a #> b #> (Param {v: r, matcher: _})) = do
    let s = store unit
    { fun: (\a2 b2 -> r `const` validateWithStoreParams s (a #> b) (param a2 :> param b2)),
      verifier: verifier s.argsList (\list (a2 #> b2) -> _verify list (a2 #> b2)) }
else
instance instanceMockParams2 :: (Show a, Eq a) 
  => MockBuilder (Param a #> Param r) (a -> r) (Param a) where
  mock (a #> Param {v: r, matcher: _}) = do
    let s = store unit
    { fun: (\a2 -> r `const` validateWithStoreParams s a (param a2)),
      verifier: verifier s.argsList (\list a2 -> _verify list a2) }

type Matcher v = v -> v -> Boolean

anyMatcher :: forall a. a -> a -> Boolean
anyMatcher _ _ = true

newtype Param v = Param {
  v :: v,
  matcher :: Maybe (Matcher v)
}

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

-- infixr 8 type Param as :
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

class VerifyCountBuilder v a where
  verifyCount :: forall m r. MonadThrow Error m => Eq v => { verifier :: Verifier v | r} -> a -> Int -> m Unit

instance instanceVerifyCountBuilderParam1 :: Eq a => VerifyCountBuilder (Param a) a where
  verifyCount {verifier : (Verifier { calledParamsList })} a count =  yyy calledParamsList (param a) count
else
instance instanceVerifyCountBuilder :: VerifyCountBuilder a a where
  verifyCount {verifier : (Verifier { calledParamsList })} v count = yyy calledParamsList v count

yyy :: forall v m. MonadThrow Error m => Eq v => CalledParamsList v -> v -> Int -> m Unit
yyy calledParamsList v count = 
  let
    callCount = length (filter (\args -> args == v) calledParamsList)
  in if count == callCount then pure unit
    else fail $ joinWith "\n" ["Function was not called the expected number of times.",  "expected: " <> show count, "but was : " <> show callCount]

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