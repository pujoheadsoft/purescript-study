module Example.Free.Teletype2 ( main )
  where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Debug (trace)

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

main :: Effect Unit
main = do
  trace(runReader readWithPlus "hoge") \_ -> log ""
