module Test.Mock
  (
  Mock,
  class MockBuilder,
  thenReturn,
  mock,
  MockArgs,
  verify,
  fun,
  Cons(..),
  (:)
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (filter, foldl, length)
import Data.String (joinWith)
import Effect.Exception (Error)
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

class MockBuilder a b r | a -> b, a -> r, b -> r where
  thenReturn :: a -> b -> Mock r

data Cons a b = Cons a b

infixr 8 type Cons as :
infixr 8 Cons as :

foreign import store :: Unit -> EqResultsStore

newtype MockArgs a = MockArgs a

instance instanceMockArgs3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c) => MockBuilder (MockArgs (Cons a (Cons b c))) r (a -> b -> c -> r) where
  thenReturn (MockArgs (Cons a (Cons b c))) r = store unit # build
    where build s = Mock { results: s.results, fn: (\a2 b2 c2 -> r `const` storeEqResults s [eqResult a a2, eqResult b b2, eqResult c c2]) }
else
instance instanceMockArgs2 :: (Show a, Eq a, Show b, Eq b) => MockBuilder (MockArgs (Cons a b)) r (a -> b -> r) where
  thenReturn (MockArgs (Cons a b)) r = store unit # build
    where build s = Mock { results: s.results, fn: (\a2 b2 -> r `const` storeEqResults s [eqResult a a2, eqResult b b2]) }
else
instance instanceMockArgs1 :: (Show a, Eq a) => MockBuilder (MockArgs a) r (a -> r) where
  thenReturn (MockArgs a) r = store unit # build
    where build s = Mock { results: s.results, fn: (\a2 -> r `const` storeEqResults s [eqResult a a2]) }

eqResult :: forall a. Eq a => Show a => a -> a -> EqResult
eqResult a1 a2 = EqResult {expected: show a1, actual: show a2, result: a1 == a2}

storeEqResults :: EqResultsStore -> EqResults -> Unit
storeEqResults s = foldl (\_ e -> s.store e) unit

mock :: forall a. a -> MockArgs a
mock = MockArgs

fun :: forall f. Mock f -> f
fun (Mock {fn}) = fn

verify :: forall a m. MonadThrow Error m => Mock a -> m Unit
verify (Mock {results}) = 
  let
    isAllMatch = all (\(EqResult {result}) -> result) results
  in 
  if isAllMatch then pure unit
  else fail $ message results

all :: forall a. (a -> Boolean) -> Array a -> Boolean
all fn arr = (length $ filter fn arr) == (length arr)

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: Array EqResult -> String
message arr = 
  let
    expecteds = arr <#> (\(EqResult arg) -> arg.expected) # joinWith ", "
    actuals = arr <#> (\(EqResult arg) -> arg.actual) # joinWith ", "
  in joinWith "\n" ["Function was not called with expected arguments.",  "expected: " <> expecteds, "but was : " <> actuals]
