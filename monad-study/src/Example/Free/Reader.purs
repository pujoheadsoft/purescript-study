module Example.Free.Reader
  where

import Prelude

import Data.Either (Either(..))
import Debug (trace)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafeCrashWith)
import Study.Control.Monad.Free (Free(..), FreeView(..), liftF, runFree, toView, resume)

newtype Reader r e = Reader (r -> e)

derive newtype instance functorReader :: Functor (Reader r)
derive newtype instance applyReader :: Apply (Reader r)
derive newtype instance applicativeReader :: Applicative (Reader r)
derive newtype instance bindReader :: Bind (Reader r)

ask :: forall r. Reader r r
ask = (Reader identity)

readWithPlus :: Reader String String
readWithPlus = do
  value <- ask
  pure (value <> " Added!")

runReader :: forall e. Reader e e -> e -> e
runReader (Reader r) e = r e

newtype Reader2 r e = Reader2 (r -> e)
derive newtype instance functorReader2 :: Functor (Reader2 r)
type FreeReader r a = Free (Reader2 r) a

ask2 :: forall e. FreeReader e e
ask2 = liftF (Reader2 identity)

readWithPlus2 :: FreeReader String String
readWithPlus2 = do
  value <- ask2
  pure (value <> " Added2!")

runReader2 :: forall e. (Free (Reader2 e) e) -> e -> e
runReader2 f e = case resume f of
  Left (Reader2 r) -> runReader2 (r e) e
  Right a -> a

main :: Effect Unit
main = do
  log $ runReader readWithPlus "hoge1"
  log $ runReader2 readWithPlus2 "hoge2"