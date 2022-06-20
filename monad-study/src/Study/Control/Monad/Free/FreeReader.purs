module Study.Control.Monad.Free.FreeReader where

import Prelude

import Data.Either (Either(..))
import Study.Control.Monad.Free.Free (Free, liftF, resume)

newtype ReaderF r e = ReaderF (r -> e)
derive newtype instance functorReaderF :: Functor (ReaderF r)
type FreeReader r a = Free (ReaderF r) a


ask :: forall e. FreeReader e e
ask = liftF (ReaderF identity)

local :: forall r a. (r -> r) -> FreeReader r a -> FreeReader r a
local f r = map f ask >>= pure <<< runReader r
-- askで取得した値をfでmapしたものを、bindで渡す
-- 渡されるのは環境の型で、かつeta変換でrunReader関数の第二引数に渡る
-- runReaderの結果は型aの値なのでpureでFreeReader型にしてやって返す

{-
↑はこう書くのと同じ
local :: forall r a. (r -> r) -> FreeReader r a -> FreeReader r a
local f r = do 
  v <- map f ask 
  pure $ runReader r v
-}

asks :: forall e a. (e -> a) -> FreeReader e a
asks f = f <$> ask

runReader :: forall r a. FreeReader r a -> r -> a
runReader f e = case resume f of
  Left (ReaderF r) -> runReader (r e) e
  Right a -> a
