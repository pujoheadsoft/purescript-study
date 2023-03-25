module Mock where

import Prelude

import Debug (debugger)
import Effect (Effect, foreachE)
import Effect.Class.Console (logShow)

newtype VerifyArg = VerifyArg { expected :: String, actual :: String, eqResult :: Boolean}

type Store = {
  args :: Array VerifyArg,
  save :: VerifyArg -> Unit
}

type Mock fn = {
  fn :: fn,
  args :: Array VerifyArg
}

verify :: Array VerifyArg -> Effect Unit
verify args = foreachE args (\(VerifyArg arg) -> logShow arg.eqResult)

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