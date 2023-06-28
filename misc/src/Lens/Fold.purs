module Lens.Fold where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Newtype (class Newtype, unwrap)
import Lens.Internal.Forget (Forget(..))
import Lens.Types (Fold)
import Safe.Coerce (coerce)


{-
  `Getter`の`view`と似ているが、こちらは取得できないかもしれない値に対応している
  展開するとこうなる
  (Forget (a -> (First a)) -> Forget (s -> (First a))) -> s -> Maybe a

  `(First <<< Just)`は、`(a -> (First a))`の型と合うので、`foldMapOf p`に渡せる。
  `foldMapOf`の戻り値の型は`(s -> (First a))`なので、s`を渡すと`First a`が返ってくる。
  それを`unwrap`すると`Maybe a`になる。
-}
preview :: forall s t a b. Fold (First a) s t a b -> s -> Maybe a
preview p s = (unwrap <<< foldMapOf p (First <<< Just)) s

-- `preview`の引数をflipしただけのもの
previewOn :: forall s t a b. s -> Fold (First a) s t a b -> Maybe a
previewOn s p = preview p s

infixl 8 previewOn as ^?

-- | 展開するとこう
-- | (Forget (a -> r) -> Forget (s -> r)) -> (a -> r) -> s -> r
-- | `s -> r`の部分を`(s -> r)`と考えると、ちょうど`(a -> r)`と対になり、`under`の戻り値と考えられるようになる。
foldMapOf :: forall s t a b r. Fold r s t a b -> (a -> r) -> s -> r
foldMapOf = under Forget


{-
  ほんとは Data.Newtypeにある関数、コメント書きたかったから書いた
  `Forget`に特殊化したものを`under'`として↓に書いてある
  `(a -> t)`の部分は、コンストラクタを受け取ってる感じ。
  ただ`_`としており、使ってはいない。
  これは`(a -> t) -> (t -> s)`の`t`の制約をつけるためだけに存在するっぽい。
  `Newtype`と`coerce`の定義から、`coerce ts`すると`t -> s`は単なる`(a -> b)`として扱えるようになるので
  `a`を渡して`b`が返される。
-}
under
  :: forall t a s b
   . Newtype t a
  => Newtype s b
  => (a -> t)
  -> (t -> s)
  -> a
  -> b
under _ ts a = (coerce ts) a


under'
  :: forall t a s b r x y
   . Newtype (Forget r a b) x
  => Newtype (Forget r s t) y
  => (x -> (Forget r a b))
  -> ((Forget r a b) -> (Forget r s t))
  -> x
  -> y
under' _ f x = (coerce f) x