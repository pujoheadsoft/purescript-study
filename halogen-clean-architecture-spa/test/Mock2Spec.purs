module Test.Mock2Spec where

import Prelude

import Data.Array (cons, foldl)
import Data.Exists (Exists, mkExists, runExists)
import Data.String (joinWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

newtype Data a = Data {value :: a -> Boolean}

class HList r  where
  hlist :: Array (Exists Data) -> r

instance instanceHListArray :: HList (Array (Exists Data)) where
  hlist = identity

-- instance instanceHList1 :: HList Int Int where
--   hlist arr = case head arr of
--     Just x -> x
--     Nothing -> -1

{-
  なんかの配列を受け取って、関数を返す
  hlist :: Array a -> (a -> r)
-}
instance instanceHListRecursive :: (Eq a, HList r) => HList (a -> r) where
  hlist xs = (\x -> hlist (cons (mkExists (Data {value: (\y -> x == y)})) xs))

list :: forall r. HList r => r
list = hlist []

-- execute :: forall r a. Semiring r => HList a (Int -> Int -> Int -> Int -> r) => r
-- execute = foldl (+) (list 1 2 3 4) []

-- execute2 ∷ ∀ a r. HList a (Int -> String -> Int -> Array String -> r) ⇒ r
execute2 :: forall r. HList r => r
execute2 =  list 1 "a" 3 ["ll"]

-- execute3 :: forall r. HList Int r => r
-- -- execute3 :: Array Int こうできる
execute3 :: forall r. HList r => r
execute3 = list 1 "2" 3

execute4 :: forall r. HList r => r -> String
execute4 r = "hoge"

-- execute5 :: forall a. Array (Exists Data) -> Array (a -> Boolean)
-- execute5 arr = (\e -> runExists (\(Data x) -> x.value) e) <$> arr

spec :: Spec Unit
spec = do
  describe "Typeaxxxble" do
    it "can handle primitives" do
      let
        -- 型が同じなら単なる配列として扱える
        x = execute3 :: Array (Exists Data)
      "" `shouldEqual` "a,a,a"

-- spec :: Spec Unit
-- spec = do
--   describe "Typeaxxxble" do
--     it "can handle primitives" do
--       let
--         -- 型が同じなら単なる配列として扱える
--         a = list 1.0 2.0 3.0 :: Array Number
--         b = list "a" "b" "c" :: Array String
--       show a `shouldEqual` "[3.0,2.0,1.0]"
--       show b `shouldEqual` "[\"c\",\"b\",\"a\"]"