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
local = \f r -> map f ask >>= flip runLocal r
  where
  runLocal e r = case resume r of
    Left (ReaderF k) ->
      runLocal e (k e)
    Right a ->
      pure a

asks :: forall e a. (e -> a) -> FreeReader e a
asks f = f <$> ask

runReader :: forall r a. FreeReader r a -> r -> a
runReader f e = case resume f of
  Left (ReaderF r) -> runReader (r e) e
  Right a -> a
