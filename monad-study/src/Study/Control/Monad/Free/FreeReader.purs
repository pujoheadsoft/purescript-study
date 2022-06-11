module Study.Control.Monad.Free.FreeReader where

import Prelude

import Data.Either (Either(..))
import Study.Control.Monad.Free (Free, liftF, resume)

newtype ReaderF r e = ReaderF (r -> e)
derive newtype instance functorReaderF :: Functor (ReaderF r)
type FreeReader r a = Free (ReaderF r) a

askFree :: forall e. FreeReader e e
askFree = liftF (ReaderF identity)

runReaderFree :: forall e. (Free (ReaderF e) e) -> e -> e
runReaderFree f e = case resume f of
  Left (ReaderF r) -> runReaderFree (r e) e
  Right a -> a
