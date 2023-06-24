module Lens.Getter where

import Prelude

import Data.Newtype (unwrap)
import Lens.Internal.Forget (Forget(..))
import Lens.Types (AGetter)


infixl 8 viewOn as ^.

-- | View the focus of a `Getter`.
-- |
-- | `AGetter`の定義から`l`は Forget (a -> a) -> Forget (s -> a)
-- | この`l に Forget identity を渡すと Forget (s -> a) が返ってくる。
-- | それを`unwrap`しているので、最終的には s -> a という関数が返る。
-- | なので`a`を返しているわけではない。
-- | 当然`s`まで渡せば`a`が返る。
view :: forall s t a b. AGetter s t a b -> s -> a
view l = unwrap (l (Forget identity))

-- | `view`をflipしただけの関数
viewOn :: forall s t a b. s -> AGetter s t a b -> a
viewOn s l = view l s
