module Util where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Type.Equality (class TypeEquals, to)
import Undefined (undefined)


class Converter input mid output | input -> output, input -> mid, mid -> output, mid -> input, output -> input where
  convert :: input -> mid -> output

extract :: forall r x a. TypeEquals r x => r -> (x -> a) -> a
extract r f = f $ to r

-- rから取り出したxから目的の関数を取り出す部分、最終的には目的の関数と同じ引数を受ける関数を返す、その関数の中でやることは自由にしたい
-- (x -> arg -> mid) (r -> mid -> out) (arg -> out)
-- func a1 = _f ReaderT a1
-- instance c3 :: TypeEquals r x => Converter (x -> a1 -> a2 -> a3 -> m a) (a1 -> a2 -> a3 -> r -> m a) where
--   convert f a1 a2 a3 = \r -> extract r f a1 a2 a3
-- else
-- instance c2 :: TypeEquals r x => Converter (x -> a1 -> a2 -> mid) (r -> mid -> out) (a1 -> a2 -> out) where
--   convert f gen = undefined
-- else
-- (x -> String -> Effect Unit) (r -> Effect Unit -> ReaderT r Effect Unit) (String -> ReaderT r Effect UNit)
instance c1 :: TypeEquals r x => Converter (x -> a -> mid) ((r -> mid) -> out) (a -> out) where
  convert f gen = undefined


class Monad m <= T m where
  a1 :: String -> m Unit
  --a2 :: String -> String -> m Unit
  -- a3 :: String -> String -> String -> m Unit


type TFunction m = {
  a1 :: String -> m Unit,
  a2 :: String -> String -> m Unit,
  a3 :: String -> String -> String -> m Unit
}



--conv = convert (\(f :: TFunction Effect) -> f.a1) (\(f :: String -> Effect Unit) -> ReaderT f) ""

-- instance instanceT 
--   :: (Monad m, TypeEquals f (TFunction m))
--   => T (ReaderT f m) where
--   a1 a = (convert (\(f :: TFunction m) -> f.a1) (\f -> ReaderT f)) a
--   --a2 a b = convert _.a2 a b # ReaderT
--   -- a3 = convert _.a3


