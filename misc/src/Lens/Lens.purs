module Lens.Lens where


import Data.Profunctor (dimap)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..))
import Lens.Types (Lens)

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

  Profunctorの`dimap`とStrongの`first`が使われているのでまず定義を示す
  [dimap]
  class Profunctor p where
    dimap :: forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d

  [first]
  class Profunctor p <= Strong p where
    first :: forall a b c. p a b -> p (Tuple a c) (Tuple b c)

  これらは型クラスの関数で、`Lens`の実体は`Profunctor p => p a b -> p s t`なので、型変数`p`がどの型になるかによって動作が変わってくる。
  
  【`p`が`->`の場合】
  `p`が関数`->`の場合、`first pab`の結果はこういう関数になる
  (Tuple a c) -> (Tuple b c)

  `->`の場合`dimap`は単なる関数合成と同じ実装になっているので、`to`や`first pab`とあわせるとこうなる(型変数名は定義にあわせた)。
  (s -> Tuple a (b -> t)) >>> ((Tuple a f) -> (Tuple b f)) >>> ((Tuple b f) -> t)

  つまり`s`を受けて、`t`を返す関数になっている。
  `b -> t`という関数は`f`として最後の関数までそのまま引き回されて、実装では`f b`の部分で`t`への変換で使われている。  

  【`p`が`Forget r`の場合】
  まず定義から
  [dimap]
  instance profunctorForget :: Profunctor (Forget r) where
    dimap :: forall a b c d. (a -> b) -> (c -> d) -> Forget r b c -> Forget r a d
    dimap f _ (Forget z) = Forget (z <<< f)
  
  [first]
  instance strongForget :: Strong (Forget r) where
    first :: forall a b c. Forget r a b -> Forget r (Tuple a c) (Tuple b c)
    first (Forget z) = Forget (z <<< fst)
  
  `dimap`の`_`にあたる部分が、`(\(Tuple b f) -> f b)`なのでこれは無視されてこうなる。
  Forget ((s -> Tuple a (b -> t)) -> ((Tuple a b) -> r))

  一部実装を交えてみるとこう。`b -> t`は無視されており、関数`z`の結果が返ることがわかる。
  Forget ((s -> Tuple a (b -> t)) -> (\(Tuple a _) -> z a))

  この関数`z`は、例えばGetterの`view`関数の場合は`identity`なので、`a`がそのまま返される。
  後述するが、無視された`(\(Tuple b f) -> f b)`の部分は`set`の部分なので、`Lens`の`set`は使われない。

  【`lens`の`get`と`set`が、どのタイミングで呼ばれているか】
  上記の関数合成の部分を見てみると、`get`は最初の関数の`(s -> Tuple a (b -> t))`の`a`の部分を取得する際に使われることがわかる。
  また`set`は最初の関数`(s -> Tuple a (b -> t))`の`(b -> t)`の部分であるが、これは関数なので最初の関数が実行されるときは呼ばれない。
  `(b -> t)`がどこで実行されるか追っていくと。関数合成の最後の関数`(\(Tuple b (b -> t)) -> (b -> t) b)`で使われている。
  実装でいうと`(\(Tuple b f) -> f b)`の`f b`の部分。
  なので`set`は関数合成の最後の関数で呼ばれる。

  擬似的なコードだとこんな感じか。
      (s -> Tuple (get s) (b -> t))
  >>> ((Tuple a (b -> t)) -> (Tuple b (b -> t)))
  >>> ((Tuple b (b -> t)) -> (\b -> set s b) b)
-}
lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)

