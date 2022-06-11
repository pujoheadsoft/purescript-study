module Study.Control.Monad.Free.FreeReader where

import Prelude

import Data.Either (Either(..))
import Study.Control.Monad.Free (Free, liftF, resume)

newtype ReaderF r e = ReaderF (r -> e)
derive newtype instance functorReaderF :: Functor (ReaderF r)
type FreeReader r a = Free (ReaderF r) a

ask :: forall e. FreeReader e e
ask = liftF (ReaderF identity)

runReader :: forall e. (Free (ReaderF e) e) -> e -> e
runReader f e = case resume f of
  Left (ReaderF r) -> runReader (r e) e
  Right a -> a
