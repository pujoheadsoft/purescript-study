module Lens.Lens where

import Prelude

import Data.Profunctor (dimap)
import Data.Profunctor.Strong (class Strong, first)
import Data.Tuple (Tuple(..))
import Lens.Types (Lens)
import Undefined (undefined)

{-
  中でlens'を呼び出している。
  lens'の定義はこう。
  lens' \s -> (Tuple (get s) \b -> set s b)

  これを get と set の内容で置き換えるとlens' の求めている関数の型と一致する。
  (関数なので実行されるまではこうならないけど)
  lens' \s -> (Tuple a (b -> t))
  
  つまり lens' の期待する↓の関数に対して get は a を取得するもの、set は t を取得するもの。
  (s -> Tuple a (b -> t))

  lens'は`to`と`pab`2つの引数を受け取っているが、`to`しか渡していないので、
  `Lens s t a b` すなわち `forall p. Strong p => p a b -> p s t`
  が返る。
-}
lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = lens' (\s -> (Tuple (get s) \b -> set s b))


{-
  ムズいので解説。
  (引数が1つに見えるが、2つ渡されていてコンパイルエラーにもならないのが不思議に思えた)

  Lensの定義はこうなっている。
  type Lens s t a b = forall p. Strong p => Optic p s t a b
  type Optic p s t a b = p a b -> p s t

  この定義を元に Lens s t a b の部分を展開するとこうなる
  lens' :: forall p s t a b. Strong p => (s -> Tuple a (b -> t)) -> p a b -> p s t

  こうすると2つ目の引数pabはまんま p a b であったことがわかる。

  だから to だけでなく pab まで引数を渡したら p s t が返ってくるけど、pab まで渡さなかったら
  p a b -> p s t が返ってくるわけ。
  んで、`lens`は`to`しか渡してないので、p a b -> p s t すなわち Lens s t a b が返ってきている。

  ---------------------------------------------------------
  コードの解説

  まずfirstの定義はこう。
  first :: forall a b c. p a b -> p (Tuple a c) (Tuple b c)

  例えば`p`が関数`->`の場合、`first pab`の結果はこういう関数になる
  (Tuple a c) -> (Tuple b c)

  dimapはいうてみれば関数合成なので結果はこうなる。
  (s -> Tuple a (b -> t)) >>> ((Tuple a c) -> (Tuple b c)) >>> (\(Tuple b f) -> f b)
  
  上記は、定義と実際の関数が混在しているが、もうちょっと代入などしてわかりやすくするとこうなる  
  (s -> Tuple a (b -> t)) >>> ((Tuple a (b -> t)) -> (Tuple b (b -> t))) >>> (\(Tuple b (b -> t)) -> (b -> t) b)

  追っていくと、s は 最終的には t になるな。
  Tuple の a も途中で b になっている

  [`lens`の`get`と`set`が、どのタイミングで呼ばれているか]
  上記の関数合成の部分を見てみると、`get`は最初の関数の`(s -> Tuple a (b -> t))`の`a`の部分を取得する際に使われることがわかる。
  また`set`は最初の関数`(s -> Tuple a (b -> t))`の`(b -> t)`の部分であるが、これは関数なので最初の関数が実行されるときは呼ばれない。
  `(b -> t)`がどこで実行されるか追っていくと。関数合成の最後の関数`(\(Tuple b (b -> t)) -> (b -> t) b)`で使われている。
  実装でいうと`(\(Tuple b f) -> f b)`の`f b`の部分。
  なので`set`は関数合成の最後の関数で呼ばれる。

  擬似的なコードだとこんな感じか。
      (s -> Tuple (get s) (b -> t))
  >>> ((Tuple a (b -> t)) -> (Tuple b (b -> t)))
  >>> ((Tuple b (b -> t)) -> (\b -> set s b) b)

  途中の関数は、`lens`や`lens'`で実装されている関数なので、関数の外側からコントロールできるのは
  `get`, `set` と `pab` の部分となる。
  `get`と`set`を指定して`Lens`を作ったあとは、`pab`で操作することになる。
  なので結構この`pab`の部分はキモとなっていると思う。
  `pab`の`first`を呼び出しているので、`first`もか。

  `pab`は `Strong p => p a b`という定義なので、`p`としてどのインスタンスが選択されるかで動きが変わる。
  例えば、`Lens`を`Getter`として`view`関数で使った場合、`Getter`の定義から`p`は`Forget`になる。
  更に`view`では`pab`として`Forget identity`が渡される。
  `Forget r`の`first`は`Forget (\(Tuple a _) -> z a)`で、`z`は`a -> r`という関数。
  この場合の`a`は`get`で取得した`a`。これが`z`に渡される。`z`は`identity`だったので`a`がそのまま返る。

  しかし`a`はそのまま`(\(Tuple b f) -> f b)`に渡せる保証がないな？
  いや`Forget`の`dimapが`よばれるんだ。そっちを見なくては。
-}
lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)

