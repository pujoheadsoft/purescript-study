module Example.Free.FreeExample where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Study.Control.Monad.Free.Free (Free, foldFree, liftF)

data HTMLElement a
  = Div String a
  | P String a

div :: String -> Free HTMLElement Unit
div v = liftF $ Div v unit

p :: String -> Free HTMLElement Unit
p v = liftF $ P v unit

content :: Free HTMLElement Unit
content = do
  div "Start"
  p "Content"
  div "End"

go :: forall a. HTMLElement a -> Effect a
go (Div v u) = do 
  log $ "<div>" <> v <> "</div>"
  pure u
go (P v u) = do
  log $ "<p>" <> v <> "</p>"
  pure u

run :: forall a. Free HTMLElement a -> Effect a
run = foldFree go

main :: Effect Unit
main = do
  run content