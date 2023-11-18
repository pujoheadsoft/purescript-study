module TaglessFinal.BadExample where

import Prelude

import Control.Monad.State (class MonadState, modify_)
import Effect.Aff (Aff, Milliseconds(..), delay)

{-
  SmallExampleと同じ型クラスを使い、GetterのインスタンスをAffにした例。
  この場合型クラスの合成自体はできるが、次のエラーが出て execStateT で実行ができない。

  No type class instance was found for

    TaglessFinal.BadExample.Getter (StateT
                                      { value :: String
                                      }
                                      Aff
                                   )
-}

class Monad m <= Getter m where
  getValue :: m String

class Monad m <= Modifier m where
  modify :: String -> m Unit

instance getterInstance :: Getter Aff where
  getValue = do
    delay (Milliseconds 1000.0)
    pure "exampleValue"

instance modifierInstance :: MonadState { value :: String } m => Modifier m where
  modify v = modify_ \state -> state { value = v }

program :: forall m. Getter m => Modifier m => m Unit
program = do
  result <- getValue
  modify result

-- コンパイルエラーになるのでコメントアウト
-- main :: Effect Unit
-- main = launchAff_ $ do
--   result <- execStateT program {value: ""} 
--   log result.value