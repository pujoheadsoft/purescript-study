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

handle :: forall a. HTMLElement a -> Effect a
handle (Div v u) = do 
  log $ "<div>" <> v <> "</div>"
  pure u
handle (P v u) = do
  log $ "<p>" <> v <> "</p>"
  pure u

run :: forall a. (HTMLElement ~> Effect) -> Free HTMLElement a -> Effect a
run f x = (foldFree f) x -- foldFreeは自然変換の関数を受け取るので↑みたいに宣言しなければならない

main :: Effect Unit
main = do
  run handle content -- handleを差し替えれば動きを変えられる。mockに変えられそう。