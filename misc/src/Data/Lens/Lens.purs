module Data.Lens.Lens where


import Data.Lens.Types (Lens)
import Data.Profunctor (dimap)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..))


lens :: forall s t a b. (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = lens' \s -> Tuple (get s) \b -> set s b


{-

  ムズいので解説。
  (引数が1つに見えるが、2つ渡されていてコンパイルエラーにもならないのがムズい)

  Lensの定義はこうなっている。
  type Lens s t a b = forall p. Strong p => Optic p s t a b
  type Optic p s t a b = p a b -> p s t

  この定義を元に Lens s t a b の部分を展開するとこうなる
  lens' :: forall p s t b a. Strong p => (s -> Tuple a (b -> t)) -> p a b -> p s t

  こうすると2つ目の引数pabはまんま p a b であったことがわかる。
  ---------------------------------------------------------
  コードの解説

  まずfirstの定義はこう。
  first :: forall a b c. p a b -> p (Tuple a c) (Tuple b c)
  これは Tuple の最初の要素だけ変換する関数を返す。

  なので (first pab) の結果はこういう関数になる
  (Tuple a c) -> (Tuple b c)

  dimapはいうてみれば関数合成なので結果はこうなる。
  (s -> Tuple a (b -> t)) >>> ((Tuple a c) -> (Tuple b c)) >>> (\(Tuple b f) -> f b)
  
  上記は、定義と実際の関数が混在しているが、もうちょっと代入などしてわかりやすくするとこうなる  
  (s -> Tuple a (b -> t)) >>> ((Tuple a (b -> t)) -> (Tuple b (b -> t))) >>> (\(Tuple b (b -> t)) -> (b -> t) b)

  追っていくと、s は 最終的には t になるな。
  Tuple の a も途中で b になっている

-}
lens' :: forall s t a b. (s -> Tuple a (b -> t)) -> Lens s t a b
lens' to pab = dimap to (\(Tuple b f) -> f b) (first pab)

