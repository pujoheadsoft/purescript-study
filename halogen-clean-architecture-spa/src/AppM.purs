module AppM where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Safe.Coerce (coerce)

newtype AppM a = AppM (Aff a)

-- runAppM :: forall q i o. H.Component q i o AppM -> Aff (H.Component q i o Aff)
-- runAppM component = pure component