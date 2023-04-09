module Test.Mock2
  (
  Cons(..),
  (:),
  calledWith,
  returns,
  Mock,
  Definition,
  class MockBuilder,
  mock,
  fun,
  verify,
  verifyCount,
  calledWithReturns,
  (:>)
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (filter, find, foldl, index, length)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect.Exception (Error, throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.Spec.Assertions (fail)
import Undefined (undefined)

newtype ArgEqResult = ArgEqResult { expected :: String, actual :: String, result :: Boolean}
type ArgsEqResult = Array ArgEqResult
type ArgsEqResults = Array ArgsEqResult

type ArgEqResultsStore = {
  results :: ArgsEqResults,
  store :: ArgsEqResult -> Unit
}

newtype Mock a = Mock {
  fn :: a,
  results :: ArgsEqResults
}

data Cons a b = Cons a b

infixr 8 type Cons as :
infixr 8 Cons as :

data Definition a r = Definition a r

calledWith :: forall a. a -> Definition a Unit
calledWith a = Definition a unit

returns :: forall a r. Definition a Unit -> r -> Definition a r
returns (Definition a _) r = Definition a r

calledWithReturns :: forall a r. a -> r -> Definition a r
calledWithReturns = calledWith >>> returns

infixr 9 calledWithReturns as :>

foreign import store :: Unit -> ArgEqResultsStore

-- class Verifier r a where
--   verify2 :: forall m. MonadThrow Error m => Mock r -> a -> m Unit

-- instance instanceVerifierArgs1 :: (Show a, Eq a) => Verifier r a where
--   verify2 (Mock {results}) a = 
--     let
--       fst = index results 0
--       _ = case fst of
--         (Just result) -> result
--         Nothing -> []
--     in pure unit

class MockBuilder a r | a -> r where
  mock :: a -> Mock r

-- instance instanceMockArrayArgs3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c) => MockBuilder (Array (Definition (a : b : c) r)) (a -> b -> r) where
--   mock a = undefined
-- else
-- instance instanceMockArrayArgs2 :: (Show a, Eq a, Show b, Eq b) => MockBuilder (Array (Definition (a : b) r)) (a -> b -> r) where
--   mock a = undefined
-- else
instance instanceMockArrayArgs1 :: (Show a, Eq a) => MockBuilder (Array (Definition a r)) (a -> r) where
  mock defs = let
    s = store unit
    fn = (\a2 -> let
      x = find (\(Definition a _) -> a == a2) defs
      y = case x of
        Just (Definition _ r) -> r
        Nothing -> error "no answer found."
      in y
    )
    in Mock { results: s.results, fn: fn }
else
instance instanceMockArgs3 :: (Show a, Eq a, Show b, Eq b, Show c, Eq c) => MockBuilder (Definition (a : b : c) r) (a -> b -> c -> r) where
  mock (Definition (Cons a (Cons b c)) r) = store unit # build
    where build s = Mock { results: s.results, fn: (\a2 b2 c2 -> r `const` validateWithStoreArgEqResults [eqResult a a2, eqResult b b2, eqResult c c2] s) }
else
instance instanceMockArgs2 :: (Show a, Eq a, Show b, Eq b) => MockBuilder (Definition (a : b) r) (a -> b -> r) where
  mock (Definition (Cons a b) r) = store unit # build
    where build s = Mock { results: s.results, fn: (\a2 b2 -> r `const` validateWithStoreArgEqResults [eqResult a a2, eqResult b b2] s) }
else
instance instanceMockArgs1 :: (Show a, Eq a) => MockBuilder (Definition a r) (a -> r) where
  mock (Definition a r) = store unit # build
    where build s = Mock { results: s.results, fn: (\a2  -> r `const` validateWithStoreArgEqResults [eqResult a a2] s) }

eqResult :: forall a. Eq a => Show a => a -> a -> ArgEqResult
eqResult a1 a2 = ArgEqResult {expected: show a1, actual: show a2, result: a1 == a2}

validateWithStoreArgEqResults :: ArgsEqResult -> ArgEqResultsStore -> Unit
validateWithStoreArgEqResults = storeArgsEqResult <<< valdateArgsEqResult

valdateArgsEqResult :: ArgsEqResult -> ArgsEqResult
valdateArgsEqResult result = if (isAnyUnMatch result) then error $ message result else result

-- storeArgEqResults :: ArgsEqResult -> ArgEqResultsStore -> Unit
-- storeArgEqResults results s = foldl (\_ e -> s.store e) unit results

storeArgsEqResult :: ArgsEqResult -> ArgEqResultsStore -> Unit
storeArgsEqResult results s = s.store results

fun :: forall f. Mock f -> f
fun (Mock {fn}) = fn

verify :: forall a m. MonadThrow Error m => Mock a -> m Unit
verify (Mock {results}) =
  if 0 == length results then fail "Function has never been called"
  else if zzz results then pure unit
  else fail $ message2 results

zzz :: ArgsEqResults -> Boolean
zzz r = all isAllMatch r

verifyCount :: forall a m. MonadThrow Error m => Mock a -> Int -> m Unit
verifyCount (Mock {results}) count =
  if count == length results then pure unit
  else fail $ joinWith "\n" ["Function was not called the expected number of times.",  "expected: " <> show count, "but was : " <> show (length results)]

isAllMatch :: ArgsEqResult -> Boolean
isAllMatch = all (\(ArgEqResult {result}) -> result)

isAnyUnMatch :: ArgsEqResult -> Boolean
isAnyUnMatch r = isAllMatch r == false

all :: forall a. (a -> Boolean) -> Array a -> Boolean
all fn arr = (length $ filter fn arr) == (length arr)

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: ArgsEqResult -> String
message arr = 
  let
    expecteds = arr <#> (\(ArgEqResult arg) -> arg.expected) # joinWith ", "
    actuals = arr <#> (\(ArgEqResult arg) -> arg.actual) # joinWith ", "
  in joinWith "\n" ["Function was not called with expected arguments.",  "expected: " <> expecteds, "but was : " <> actuals]

message2 :: ArgsEqResults -> String
message2 arr = "no answer found."

error :: forall a. String -> a
error = unsafePerformEffect <<< throw