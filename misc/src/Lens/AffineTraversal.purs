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

  【`p` が `Forget r` の場合】
  `p`を`Forget r`として展開すると、このようになる。
  AffineTraversal s t a b = (Forget r a b) -> (Forget r s t)

  実体的にはこうなる。
  Forget (a -> r) -> Forget (s -> r)

  つまり引数の`pab`は`(Forget r a b)`。

  1. `Choice (Forget r)`の定義
  instance choiceForget :: Monoid r => Choice (Forget r) where
    right :: forall a b c. (Forget r b c) -> (Forget r) (Either a b) (Either a c)
    right (Forget z) = Forget (either mempty z)

  なので`right`は次の関数を返す
  forall r a c. Forget ((Either c a) -> r) 

  ちなみに`either`は今回の場合こうなる
  `Left a`だったら`empty`を返す（例えば`Maybe`だったら`empty`は`Nothing`になる）
  `Right a`だったら`(a -> r) a`を返す（つまり`r`が返る）

  2. `Strong (Forget r)`の定義
  instance strongForget :: Strong (Forget r) where
    second :: forall a b c. Forget r b c -> Forget r (Tuple a b) (Tuple a c)
    second (Forget z) = Forget ( z <<< snd)
    ↓
    second (Forget z) = Forget (\(Tuple _ a) -> z a) 
  
  3. `right`と`second`の結果
  当て込むとこう
    Forget (\(Tuple _ a) -> (z :: ((Either c a) -> r)) a) 
  なので`a`は`(Either c a)`だとわかるから
  `second`は以下を返す
    Forget ((Tuple _ (Either c a)) -> ((Either c a) -> r)) 
  
  これは`diamp a2b c2d b2c`の`b2c`の部分で、`a2b`の部分は`(s -> Tuple (b -> t) (Either t a))`なので、関数が繋がった。
  `Forget`の`demap`では`c2d`は無視される。
  ちなみに`Tuple (b -> t) (Either t a)`の`b -> t`の部分は、`set`なので`Forget`のときはセッターは無視されている。

  4. `Index`の`ix`で使われた場合（配列に対して使われた場合）
  `Index (Array a)`の場合の`pre`の定義はこう
    pre :: Array a -> Either (Array a) a
    pre s = maybe (Left s) Right $ A.index s n

  affineTraversal' :: forall s t a b  (s -> Tuple (b -> t) (Either t a)) -> (Forget r a b) -> (Forget r s t)
  ↓
  affineTraversal' :: forall s t a b  (s -> Tuple _ (Either (Array a) a)) -> (Forget r a b) -> (Forget r s (Array a))

  Forgetの`dimap`
    dimap :: forall a b c d. (a -> b) -> (c -> d) -> Forget r b c -> Forget r a d
    dimap f _ (Forget z) = Forget (z <<< f)

  今回の場合だと型変数は
    a: s
    b: Tuple (b -> t) (Either t a)
  だから型としてはこうなる。
    dimap
      :: forall s t a b c d r
       . (s -> Tuple (b -> t) (Either t a))
       -> (c -> d)
       -> Forget r (Tuple (b -> t) (Either t a)) c
       -> Forget r s d
  `Forget`の`dimap`なので、`(c -> d)`は無視される。

  これの`AffineTraversal s t a b`の部分だけに着目すると`AffineTraversal`として返るのはこういう型。
    Forget r (Tuple (b -> t) (Either t a)) c -> Forget r s d
    ↓ 配列を当て込むとこう
    Forget r (Tuple (b -> t) (Either (Array a) a)) c -> Forget r s d
    

  5. `Fold`の`preview`で `[5, 6, 7] ^? ix 2` のように使われた場合 ( `^?` は `flip preview` と同じ )
  `ix 3`の結果は上記のような関数なのでこう。
    preview p s = (unwrap <<< foldMapOf p (First <<< Just)) s
    `s` = [5, 6, 7]
    `p` = Forget r (Tuple (b -> t) (Either (Array a) a)) c -> Forget r s d

    `Forget`の`foldMapOf`はこうなる。
    (Forget (a -> r) -> Forget (s -> r)) -> (a -> r) -> s -> r

    今回の`(a -> r)`は`(First <<< Just)`である。
    冗長に書くとこう。
    ((\x -> Just x) >>> (\x -> First x)) :: x -> First x

    `p`の実体はこう。
    Forget (Tuple (b -> t) (Either (Array a) a) -> r) -> Forget (s -> r)
    ↓
    Forget (Tuple (b -> t) (Either (Array a) a) -> First r) -> Forget (s -> r)

    `First r`は`Left (Array a)`か`Right a`を元に作る。

    で、`(Either (Array a) a) -> r`の部分は、`pre`であった。
      pre s = maybe (Left s) Right $ A.index s n
    値は当て込んでみるとこうなる。
      maybe (Left s) Right $ A.index [5, 6, 7] 2
    結果は`Right 6`になる。
    この6をFirstに渡して First 6。これをunwrapしてJust 6
    これが戻り値

-}
affineTraversal'
  :: forall s t a b
   . (s -> Tuple (b -> t) (Either t a))
  -> AffineTraversal s t a b
affineTraversal' to pab =
  dimap to (\(Tuple b f) -> either identity b f) (second (right pab))
