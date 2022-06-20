module Study.Control.Monad.Free.FreeWriter where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Study.Control.Monad.Free.Free (Free, liftF, resume)

data Writer w a = Writer w a

derive instance functorWriter :: Functor (Writer w)

type FreeWriter w a = Free (Writer w) a

writer :: forall w a. Tuple a w -> FreeWriter w a
writer (Tuple a w) = liftF (Writer w a)

runWriter :: forall w a. Monoid w => FreeWriter w a -> Tuple a w
runWriter f = foldWriter (<>) mempty f

foldWriter :: forall w a. (w -> w -> w) -> w -> FreeWriter w a -> Tuple a w
foldWriter = loop
  where
  loop k w f = case resume f of
    Left (Writer w' n) ->
      loop k (k w w') n
    Right a ->
      (Tuple a w)


tell :: forall w. Monoid w => w -> FreeWriter w Unit
tell w = writer (Tuple unit w)

censor :: forall w a. Monoid w => (w -> w) -> FreeWriter w a -> FreeWriter w a
censor = loop
  where
  loop f r = case resume r of
    Left (Writer w n) -> do
      tell (f w)
      loop f n
    Right a ->
      pure a