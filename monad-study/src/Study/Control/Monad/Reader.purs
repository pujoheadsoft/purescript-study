module Study.Control.Monad.Reader
  where

import Prelude

newtype Reader r e = Reader (r -> e)

derive newtype instance functorReader :: Functor (Reader r)
derive newtype instance applyReader :: Apply (Reader r)
derive newtype instance applicativeReader :: Applicative (Reader r)
derive newtype instance bindReader :: Bind (Reader r)

ask :: forall r. Reader r r
ask = (Reader identity)

asks :: forall e a. (e -> a) -> Reader e a
asks f = f <$> ask

local :: forall r a. (r -> r) -> Reader r a -> Reader r a
local f (Reader r) = Reader $ r <<< f

withReader :: forall r1 r2 a. (r2 -> r1) -> Reader r1 a -> Reader r2 a
withReader f (Reader r) = Reader $ r <<< f

runReader :: forall r e. Reader r e -> r -> e
runReader (Reader r) e = r e
