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
  showCalledArgs,
  Arg,
  arg
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
import Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

type Mock fn v = {
  fun :: fn,
  verifier :: Verifier v
}

foreign import store :: forall a. Unit -> CallredArgsStore a

type CallredArgsStore v = {
  argsList :: CalledArgsList v,
  store :: v -> Unit
}

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

instance finderArg3 :: (Eq a, Eq b) => Finder (Arg a #> Arg b #> Arg r) (Arg a #> Arg b) r where
  findMatchedReturnValue defs (a2 #> b2) = case find (\(a #> b #> _) -> (a #> b) == (a2 #> b2)) defs of
    Just (_ #> _ #> (Arg {v: r, any: _})) -> r
    Nothing -> error "no answer found."
else
instance finderArg2 :: Eq a => Finder (Arg a #> Arg r) (Arg a) r where
  findMatchedReturnValue defs a2 = case find (\(a #> _) -> a == a2) defs of
    Just (_ #> (Arg {v: r, any: _})) -> r
    Nothing -> error "no answer found."

instance instanceMockArrayArgs3 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Array (Arg a #> Arg b #> Arg r)) (a -> b -> r) (Arg a #> Arg b) where
  mock defs = do
    let s = store unit
    { fun: (\a2 b2 -> findMatchedReturnValue defs (arg a2 :> arg b2) `const` storeCalledArgs s (arg a2 :> arg b2)),
      verifier: verifier s.argsList (\list (a2 #> b2) -> _verify list (a2 #> b2)) }
else
instance instanceMockArrayArgs2 :: (Show a, Eq a)
  => MockBuilder (Array (Arg a #> Arg r)) (a -> r) (Arg a) where
  mock defs = do
    let s = store unit
    { fun: (\a2 -> findMatchedReturnValue defs (arg a2) `const` storeCalledArgs s (arg a2)),
      verifier: verifier s.argsList (\list a2 -> _verify list a2) }
else
instance instanceMockArgs4 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c)
  => MockBuilder (Arg a #> Arg b #> Arg c #> Arg r) (a -> b -> c -> r) (Arg a #> Arg b #> Arg c) where
  mock (a #> b #> c #> (Arg {v: r, any: _})) = do
    let s = store unit
    { fun: (\a2 b2 c2 -> r `const` validateWithStoreArgs s (a #> b #> c) (arg a2 :> arg b2 :> arg c2) ),
      verifier: verifier s.argsList (\list (a2 #> b2 #> c2) -> _verify list (a2 #> b2 #> c2)) }
else
instance instanceMockArgs3 :: (Show a, Eq a, Show b, Eq b)
  => MockBuilder (Arg a #> Arg b #> Arg r) (a -> b -> r) (Arg a #> Arg b) where
  mock (a #> b #> (Arg {v: r, any: _})) = do
    let s = store unit
    { fun: (\a2 b2 -> r `const` validateWithStoreArgs s (a #> b) (arg a2 :> arg b2)),
      verifier: verifier s.argsList (\list (a2 #> b2) -> _verify list (a2 #> b2)) }
else
instance instanceMockArgs2 :: (Show a, Eq a) 
  => MockBuilder (Arg a #> Arg r) (a -> r) (Arg a) where
  mock (a #> Arg {v: r, any: _}) = do
    let s = store unit
    { fun: (\a2 -> r `const` validateWithStoreArgs s a (arg a2)),
      verifier: verifier s.argsList (\list a2 -> _verify list a2) }

newtype Arg v = Arg {
  v :: v,
  any :: Boolean
}

instance eqArg :: Eq a => Eq (Arg a) where
  eq (Arg {v: a, any: _}) (Arg {v: b, any: _}) = a == b

instance showArg :: Show a => Show (Arg a) where
  show (Arg {v, any: _}) = show v

class ConsGen a b r | a -> r, b -> r where
  cons :: a -> b -> r

instance instaneConsGen9 :: ConsGen (Cons a b) (Cons b c) (Cons (Cons a b) (Cons b c)) where
  cons = Cons
else
instance instaneConsGen8 :: ConsGen (Cons a b) (Arg b) (Cons (Cons a b) (Arg b)) where
  cons = Cons
else
instance instaneConsGen7 :: ConsGen (Arg a) (Cons b c) (Cons (Arg a) (Cons b c)) where
  cons = Cons
else
instance instaneConsGen6 :: ConsGen a (Cons b c) (Cons (Arg a) (Cons b c)) where
  cons a b = Cons (Arg {v: a, any: false}) b
else
instance instaneConsGen5 :: ConsGen (Cons a b) c (Cons (Cons a b) (Arg c)) where
  cons a b = Cons a (Arg {v: b, any: false})
else
instance instaneConsGen4 :: ConsGen (Arg a) (Arg b) (Cons (Arg a) (Arg b)) where
  cons = Cons
else
instance instaneConsGen3 :: ConsGen a (Arg b) (Cons (Arg a) (Arg b)) where
  cons a b = Cons (Arg {v: a, any: false}) b
else
instance instaneConsGen2 :: ConsGen (Arg a) b (Cons (Arg a) (Arg b)) where
  cons a b = Cons a (Arg {v: b, any: false})
else
instance instaneConsGen :: ConsGen a b (Cons (Arg a) (Arg b)) where
  cons a b = Cons (Arg {v: a, any: false}) (Arg {v: b, any: false})

arg :: forall a. a -> Arg a
arg a = Arg {v: a, any: false}

-- infixr 8 type Arg as :
infixr 8 cons as :>

_verify :: forall a. Eq a => Show a => Array a -> a -> Maybe VerifyFailed
_verify list a = 
  if any (_ == a) list then Nothing 
  else Just $ VerifyFailed ("No answer found for argument: " <> show a)

class VerifyBuilder v a where
  verify :: forall m r. MonadThrow Error m => { verifier :: Verifier v | r} -> a -> m Unit

instance instanceVerifyBuilderArg1 :: VerifyBuilder (Arg a) a where
  verify {verifier : (Verifier { calledArgsList, verify: doVerify })} a = 
    zzz doVerify calledArgsList (arg a)
else
instance instanceVerifyBuilder :: VerifyBuilder a a where
  verify {verifier : (Verifier { calledArgsList, verify: doVerify })} args = 
    zzz doVerify calledArgsList args

zzz :: forall v m. MonadThrow Error m => (CalledArgsList v -> v -> Maybe VerifyFailed) -> CalledArgsList v -> v -> m Unit
zzz doVerify calledArgsList args = case doVerify calledArgsList args of
      Just (VerifyFailed msg) -> fail msg
      Nothing -> pure unit

validateArgs :: forall a. Eq a => Show a => a -> a -> Unit
validateArgs expected actual = if (expected == actual) then unit else error $ message expected actual

storeCalledArgs :: forall a. CallredArgsStore a -> a -> a
storeCalledArgs s a = const a (s.store a)

validateWithStoreArgs :: forall a. Eq a => Show a => CallredArgsStore a -> a -> a -> Unit
validateWithStoreArgs s expected actual = validateArgs expected (storeCalledArgs s actual)

class VerifyCountBuilder v a where
  verifyCount :: forall m r. MonadThrow Error m => Eq v => { verifier :: Verifier v | r} -> a -> Int -> m Unit

instance instanceVerifyCountBuilderArg1 :: VerifyCountBuilder (Arg a) a where
  verifyCount {verifier : (Verifier { calledArgsList })} a count =  yyy calledArgsList (arg a) count
else
instance instanceVerifyCountBuilder :: VerifyCountBuilder a a where
  verifyCount {verifier : (Verifier { calledArgsList })} v count = yyy calledArgsList v count

yyy :: forall v m. MonadThrow Error m => Eq v => CalledArgsList v -> v -> Int -> m Unit
yyy calledArgsList v count = 
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






anyX :: forall a. Arg a
anyX = unsafeCoerce Arg {v: "", any: true}

anyV :: forall a. a -> Arg a
anyV a = Arg {v: a, any: true}

cc = anyX :: Arg Int

dd :: Cons (Arg String) (Cons (Arg Int) (Arg Boolean))
dd = arg "a" #> arg 1 #> arg true

dd2 :: Cons (Arg String) (Cons (Arg Int) (Cons (Arg Boolean) (Arg String)))
dd2 = arg "a" #> arg 1 #> arg true #> arg "b"

ee :: Cons (Arg String) (Arg Int)
ee = "a" :> 1

ff :: Cons (Arg String) (Cons (Arg Int) (Arg Boolean))
ff = "a" :> 1 :> true

gg :: Cons (Arg String) (Cons (Arg Int) (Cons (Arg Boolean) (Arg String)))
gg = "a" :> 1 :> true :> "b"


hh :: Cons (Arg String) (Cons (Arg Int) (Arg Boolean))
hh = anyV "a" :> anyV 1 :> anyV true