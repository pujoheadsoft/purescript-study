module Lens.AffineTraversal where

import Prelude

import Data.Either (Either, either)
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (right)
import Data.Profunctor.Strong (second, (&&&))
import Data.Tuple (Tuple(..))
import Lens.Types (AffineTraversal)


{-
  setter と preview の関数を受けて `AffineTraversal`を返す。

  `&&&`は`fanout`で同じ値に別々の関数を適用した`Tuple`を作る。
  例: `(fanout show Just) 100` ならば `Tuple "100" (Just 100)`となる

  `(s -> b -> t)` も `(s -> Either t a)` もどちらも `s` を受けるので、`fanaout`の引数として使える。
  ということなので、`affineTraversal'` の引数は `(s -> Tuple (b -> t) (Either t a))` となっている。
-}
affineTraversal
  :: forall s t a b
   . (s -> b -> t)
  -> (s -> Either t a)
  -> AffineTraversal s t a b
affineTraversal set pre = affineTraversal' (set &&& pre)

{-
  AffineTraversal s t a b を展開すると以下になる。なので、`p`は`Strong`かつ`Choice`。
  forall p. Strong p => Choice p => p a b -> p s t

  関数`to`と引数`pab`だけ受けて、`AffineTraversal s t a b`を返すというのは、`Lens`の`lens'`と同じ構造。

  [`pab` が `Forget r` の場合]

  1. `Choice (Forget r)`の定義
  instance choiceForget :: Monoid r => Choice (Forget r) where
    right :: forall a b c. (Forget r b c) -> (Forget r) (Either a b) (Either a c)
    right (Forget z) = Forget (either mempty z)

  なので`right`は次の関数を返す
  forall r a c. Forget ((Either c a) -> r) 
  
  2. `Strong (Forget r)`の定義
  instance strongForget :: Strong (Forget r) where
    second :: forall a b c. Forget r b c -> Forget r (Tuple a b) (Tuple a c)
    second (Forget z) = Forget ( z <<< snd)
    ↓
    second (Forget z) = Forget (\(Tuple _ a) -> z a) 
  
    `z` は `(Either c a) -> r` なので、
    

-}
affineTraversal'
  :: forall s t a b
   . (s -> Tuple (b -> t) (Either t a))
  -> AffineTraversal s t a b
affineTraversal' to pab =
  dimap to (\(Tuple b f) -> either identity b f) (second (right pab))