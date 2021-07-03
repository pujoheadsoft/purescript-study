module Control.Monad.Reader.MyClass where

import Prelude

class Monad m <= MonadAsk r m | m -> r where
  ask :: m r

instance monadAskFunction :: MonadAsk r ((->) r) where
  ask = identity

class MonadAsk r m <= MonadReader r m | m -> r where
  local :: forall a. (r -> r) -> m a -> m a

instance monadReaderFunction :: MonadReader r ((->) r) where
  local = (>>>)