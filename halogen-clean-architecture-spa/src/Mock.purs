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


mock1 :: forall a1 r. Eq a1 => Show a1 => a1 -> r -> Mock (a1 -> r)
mock1 a1 r = 
  let
    s = store unit
  in { args: s.args, fn: (\a2 -> 
    let
      _ = s.save $ VerifyArg {expected: show a1, actual: show a2, eqResult: a1 == a2}
    in r)
  }

mock2 :: forall a1 b1 r. Eq a1 => Show a1 => Eq b1 => Show b1 => a1 -> b1 -> r -> Mock (a1 -> b1 -> r)
mock2 a1 b1 r = 
  let
    s = store unit
  in { args: s.args, fn: (\a2 b2 -> 
    let
      _ = s.save $ VerifyArg {expected: show a1, actual: show a2, eqResult: a1 == a2}
      _ = s.save $ VerifyArg {expected: show b1, actual: show b2, eqResult: b1 == b2}
    in r)
  }

mock3 :: forall a1 b1 c1 r. Eq a1 => Show a1 => Eq b1 => Show b1 => Eq c1 => Show c1 => a1 -> b1 -> c1 -> r -> Mock (a1 -> b1 -> c1 -> r)
mock3 a1 b1 c1 r = 
  let
    s = store unit
  in { args: s.args, fn: (\a2 b2 c2 -> 
    let
      _ = s.save $ VerifyArg {expected: show a1, actual: show a2, eqResult: a1 == a2}
      _ = s.save $ VerifyArg {expected: show b1, actual: show b2, eqResult: b1 == b2}
      _ = s.save $ VerifyArg {expected: show c1, actual: show c2, eqResult: c1 == c2}
    in r)
  }

--m = moge 1 "2" true :: Mock (Int -> String -> Boolean)

{-
  aaからddまで合成していくと引数を増やすことができる
  構造としては、渡した関数をラップする引数を一つ増やした関数を返す構造になっている
  (a) -> (b -> a)
  これを再帰すれば可変長引数は実現できる
-}
-- これは別になくてもいい
aa :: forall a. a -> a
aa a = a

-- こいつが基本
-- こいつと↓のやつを動的に合成していければよさそう
-- bb >>> cc とすると引数を3つとる関数になる
bb :: forall a1 a2. a1 -> (a2 -> a1)
bb a1 = (\a2 -> a1)

cc :: forall a1 a2 a3. (a2 -> a1) -> (a3 -> a2 -> a1)
cc a2Toa1 = (\a3 -> a2Toa1)

dd :: forall a1 a2 a3 a4. (a3 -> a2 -> a1) -> (a4 -> a3 -> a2 -> a1)
dd a3a2a1 = (\a4 -> a3a2a1)

main :: Effect Unit
main = do
  let
    h = hoge 1 1 5 :: Mock (Int -> Int -> Int)
  logShow $ h.fn 1 2
  verify h.args