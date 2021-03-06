module Study.Control.Monad.Free.Class where

import Prelude

class Monad m <= MonadFree f m | m -> f where
  wrapFree :: forall a. f (m a) -> m a

-- instance monadFreeFree :: MonadFree f (Free f) where
--  wrapFree = join <<< liftf