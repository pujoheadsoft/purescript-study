module Mock where

import Prelude

import Data.Array (filter, find, length)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Debug (debugger)
import Effect (Effect, foreachE)
import Effect.Class.Console (logShow)
import Prim.Boolean (True)
import Test.Spec.Assertions (fail)

newtype VerifyArg = VerifyArg { expected :: String, actual :: String, eqResult :: Boolean}

type Store = {
  args :: Array VerifyArg,
  save :: VerifyArg -> Unit
}

type Mock fn = {
  fn :: fn,
  args :: Array VerifyArg
}

any :: forall a. (a -> Boolean) -> Array a -> Boolean
any fn arr = case find fn arr of
  Just _ -> true
  Nothing -> false

all :: forall a. (a -> Boolean) -> Array a -> Boolean
all fn arr = (length $ filter fn arr) == (length arr)

{-
  Function was not called with expected arguments.
  expected: 1, "2", 3
  but was : 1, "1", 1
-}
message :: Array VerifyArg -> String
message arr = 
  let
    expecteds = arr <#> (\(VerifyArg arg) -> arg.expected) # joinWith ", "
    actuals = arr <#> (\(VerifyArg arg) -> arg.actual) # joinWith ", "
  in joinWith "\n" ["Function was not called with expected arguments.",  "expected: " <> expecteds, "but was : " <> actuals]

verify :: Array VerifyArg -> Effect Unit
verify args = 
  let
    isAllMatch = all (\(VerifyArg arg) -> arg.eqResult) args
  in 
  if isAllMatch then pure unit
  else fail $ message args

foreign import store :: Unit -> Store

hoge :: forall a1 b1 r. Eq a1 => Show a1 => Eq b1 => Show b1 => a1 -> b1 -> r -> Mock (a1 -> b1 -> r)
hoge a1 b1 r = 
  let
    s = store unit
  in { args: s.args, fn: (\a2 b2 -> 
    let
      _ = s.save $ VerifyArg {expected: show a1, actual: show a2, eqResult: a1 == a2}
      _ = s.save $ VerifyArg {expected: show b1, actual: show b2, eqResult: b1 == b2}
    in r)
  }

main :: Effect Unit
main = do
  let
    h = hoge 1 1 5 :: Mock (Int -> Int -> Int)
  logShow $ h.fn 1 2
  verify h.args