module TaglessFinal.SmallExample where

import Prelude

import Control.Monad.State (class MonadState, execStateT, modify_)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)

{-
  Tagless Finalスタイルでの合成のシンプルな例
  型クラス Getter のインスタンスを Getter Aff のようにするのではなく、MonadAffにしているのがミソ。
  合成する型クラスのインスタンスがどれも同じであれば問題なく合成できるが、
  この例では Modifier のインスタンスが MonadState になっており、Getter のインスタンスを Aff にした場合合成できなくなる。
  正確に言えば両方使う関数は定義できるが、実行ができない（その例はBadExample.pursを参照）
  MonadAffにはStateTのインスタンスが定義されているため実行できるのだ。
-}
class Monad m <= Getter m where
  getValue :: m String

class Monad m <= Modifier m where
  modify :: String -> m Unit

instance getterInstance :: MonadAff m => Getter m where
  getValue = liftAff do
    delay (Milliseconds 1000.0)
    pure "exampleValue"

instance modifierInstance :: MonadState { value :: String } m => Modifier m where
  modify v = modify_ \state -> state { value = v }

program :: forall m. Getter m => Modifier m => m Unit
program = do
  result <- getValue
  modify result

main :: Effect Unit
main = launchAff_ $ do
  result <- execStateT program {value: ""} 
  log result.value
