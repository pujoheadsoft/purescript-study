module Study.Control.Monad.Run.Choose
  ( liftChoose
  , cempty
  , calt
  , runChoose
  , module Study.Control.Monad.Run.Internal
  ) where

import Prelude

import Control.Alternative (class Alternative, alt, empty)
import Data.Either (Either(..))
import Data.Functor.Variant (on)
import Study.Control.Monad.Run.Internal (Choose(..), CHOOSE, _choose)
import Study.Control.Monad.Run.Run (Run)
import Study.Control.Monad.Run.Run as Run
import Type.Row (type (+))

liftChoose :: forall r a. Choose a -> Run (CHOOSE + r) a
liftChoose = Run.lift _choose

cempty :: forall r a. Run (CHOOSE + r) a
cempty = empty

calt :: forall r a. Run (CHOOSE + r) a -> Run (CHOOSE + r) a -> Run (CHOOSE + r) a
calt = alt

runChoose :: forall f a r. Alternative f => Run (CHOOSE + r) a -> Run r (f a)
runChoose = loop
  where
  handle = on _choose Left Right
  loop r = case Run.peel r of
    Left a -> case handle a of
      Left a' -> case a' of
        Empty -> pure empty
        Alt k -> do
          x <- loop (k true)
          y <- loop (k false)
          pure (alt x y)
      Right a' ->
        Run.send a' >>= loop
    Right a ->
      pure (pure a)