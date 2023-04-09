module Test.Mock
  (
  Mock,
  class MockBuilder,
  thenReturn,
  mock,
  MockArgs,
  verify,
  verifyCount,
  fun,
  Cons(..),
  (:)
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (filter, foldl, length)
import Data.String (joinWith)
import Effect.Exception (Error, throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.Spec.Assertions (fail)

newtype EqResult = EqResult { expected :: String, actual :: String, result :: Boolean}
type EqResults = Array EqResult

type EqResultsStore = {
  results :: EqResults,
  store :: EqResult -> Unit
}

newtype Mock a = Mock {
  fn :: a,
  results :: EqResults
}

derive instance mockFunctor :: Functor Mock
instance applicativeMock :: Applicative Mock where
  pure a = Mock {fn: a, results: []}
instance applyMock :: Apply Mock where
  apply (Mock {fn: a2b}) (Mock {fn: a, results}) = Mock {fn: a2b a, results: results}
instance bindMock :: Bind Mock where
  bind (Mock {fn}) f = f fn
instance monadMock :: Monad Mock

class MockBuilder a b r | a -> b, a -> r, b -> r where
  thenReturn :: a -> b -> Mock r

data Cons a b = Cons a b

infixr 8 type Cons as :
infixr 8 Cons as :

foreign import store :: Unit -> EqResultsStore

newtype MockArgs a = MockArgs a

instance instanceMockArgs3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c) => MockBuilder (MockArgs (a : b : c)) r (a -> b -> c -> r) where
  thenReturn (MockArgs (Cons a (Cons b c))) r = store unit # build
    where build s = Mock { results: s.results, fn: (\a2 b2 c2 -> r `const` validateWithStoreEqResults [eqResult a a2, eqResult b b2, eqResult c c2] s) }
else
instance instanceMockArgs2 :: (Show a, Eq a, Show b, Eq b) => MockBuilder (MockArgs (a : b)) r (a -> b -> r) where
  thenReturn (MockArgs (Cons a b)) r = store unit # build
    where build s = Mock { results: s.results, fn: (\a2 b2 -> r `const` validateWithStoreEqResults [eqResult a a2, eqResult b b2] s) }
else
instance instanceMockArgs1 :: (Show a, Eq a) => MockBuilder (MockArgs a) r (a -> r) where
  thenReturn (MockArgs a) r = store unit # build
    where build s = Mock { results: s.results, fn: (\a2  -> r `const` validateWithStoreEqResults [eqResult a a2] s) }

eqResult :: forall a. Eq a => Show a => a -> a -> EqResult
eqResult a1 a2 = EqResult {expected: show a1, actual: show a2, result: a1 == a2}

validateWithStoreEqResults :: EqResults -> EqResultsStore -> Unit
validateWithStoreEqResults = storeEqResults <<< valdateEqResults

valdateEqResults :: EqResults -> EqResults
valdateEqResults results = if (isAnyUnMatch results) then error $ message results else results

storeEqResults :: EqResults -> EqResultsStore -> Unit
storeEqResults results s = foldl (\_ e -> s.store e) unit results

mock :: forall a. a -> MockArgs a
mock = MockArgs

fun :: forall f. Mock f -> f
fun (Mock {fn}) = fn

verify :: forall a m. MonadThrow Error m => Mock a -> m Unit
verify (Mock {results}) = 
  if 0 == length results then fail "Function has never been called"
  else if all (\(EqResult {result}) -> result) results then pure unit
  else fail $ message results

verifyCount :: forall a m. MonadThrow Error m => Mock a -> Int -> m Unit
verifyCount (Mock {results}) count =
  if count == length results then pure unit
  else fail $ joinWith "\n" ["Function was not called the expected number of times.",  "expected: " <> show count, "but was : " <> show (length results)]

isAllMatch :: EqResults -> Boolean
isAllMatch = all (\(EqResult {result}) -> result)

isAnyUnMatch :: EqResults -> Boolean
isAnyUnMatch r = isAllMatch r == false

all :: forall a. (a -> Boolean) -> Array a -> Boolean
all fn arr = (length $ filter fn arr) == (length arr)

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: EqResults -> String
message arr = 
  let
    expecteds = arr <#> (\(EqResult arg) -> arg.expected) # joinWith ", "
    actuals = arr <#> (\(EqResult arg) -> arg.actual) # joinWith ", "
  in joinWith "\n" ["Function was not called with expected arguments.",  "expected: " <> expecteds, "but was : " <> actuals]

error :: forall a. String -> a
error = unsafePerformEffect <<< throw