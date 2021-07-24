module Study.Control.Monad.Freer.Reader where

import Data.Exists (runExists)
import Study.Control.Monad.Freer (Freer(..), FreerBindF(..), send)

data Reader e a = Reader e e

--ask :: forall e. Freer (Reader e) e
--ask = send Reader

runReader :: forall e a. Freer (Reader e) a -> e -> a
runReader m e = case m of
  Pure x -> x
  Bind x -> runExists (\(FreerBindF (Reader a b) r) -> runReader (r Reader a) e) x