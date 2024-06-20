module Study.Control.Monad.Simple.Except where

import Prelude

import Data.Either (Either(..), either)

newtype Except e a = Except (Either e a)

runExcept :: forall e a. Except e a -> Either e a
runExcept (Except x) = x

instance functorExcept :: Functor (Except e) where
  map f (Except x) = Except (f <$> x)

instance applyExcept :: Apply (Except e) where
  apply = ap

instance applicativeExcept :: Applicative (Except e) where
  pure = Except <<< Right

instance bindExcept :: Bind (Except e) where
  bind (Except x) k =
    Except (either Left (\a -> case k a of Except b -> b) x)

instance monadExcept :: Monad (Except e)