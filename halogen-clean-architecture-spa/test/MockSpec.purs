module Test.MockSpec where

import Prelude

import Data.Array (cons, foldl)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

class HList a r | r -> a where
  hlist :: Array a -> r

instance instanceHListArray :: HList a (Array a) where
  hlist = identity

-- instance instanceHList1 :: HList Int Int where
--   hlist arr = case head arr of
--     Just x -> x
--     Nothing -> -1

{-
  なんかの配列を受け取って、関数を返す
  hlist :: Array a -> (a -> r)
-}
instance instanceHListRecursive :: (HList a r) => HList a (a -> r) where
  hlist xs = (\x -> hlist (cons x xs))

list :: forall a r. HList a r => r
list = hlist []

execute :: forall r a. Semiring r => HList a (Int -> Int -> Int -> Int -> r) => r
execute = foldl (+) (list 1 2 3 4) []

execute2 ∷ ∀ a r. HList a (Int -> String -> Int -> Array String -> r) ⇒ r
execute2 =  list 1 "a" 3 ["ll"]

execute3 :: forall r. HList Int r => r
-- execute3 :: Array Int こうできる
execute3 = list 1 2 3

spec :: Spec Unit
spec = do
  describe "Typeaxxxble" do
    it "can handle primitives" do
      let
        -- 型が同じなら単なる配列として扱える
        a = list 1.0 2.0 3.0 :: Array Number
        b = list "a" "b" "c" :: Array String
      show a `shouldEqual` "[3.0,2.0,1.0]"
      show b `shouldEqual` "[\"c\",\"b\",\"a\"]"
