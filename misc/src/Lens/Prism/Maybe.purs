module Lens.Prism.Maybe where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Lens.Prism (prism)
import Lens.Types (Prism)

_Just :: forall a b. Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right