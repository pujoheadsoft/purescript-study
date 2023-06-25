module Lens.Setter where

import Prelude

import Lens.Types (Setter)


-- | 関数を `Setter` の焦点に適用する。
-- |
-- | `Setter`を展開してみるとこうなる。
-- | ((a -> b) -> (s -> t)) -> (a -> b) -> s -> t
-- | つまり「まんま」（正確には`Setter`はくっついてるけど、バラされる感じか）
-- | `(a -> b)` と `s` を受けて `t` を返す関数を返す。
over :: forall s t a b. Setter s t a b -> (a -> b) -> s -> t
over l = l

-- | Setter`の焦点を一定の値に設定する。
-- |
-- | `over`から返される関数`(a -> b) -> s -> t`に`(const b)`と`s`を渡している。
-- | つまり`a -> b`に`const b`が、`s`には`s`が渡されて、`t`が返される。
-- | `const b`なので`a`がなんだろうと`b`はそのまま渡される。`s`もそう。
set :: forall s t a b. Setter s t a b -> b -> s -> t
set l b s = over l (const b) s