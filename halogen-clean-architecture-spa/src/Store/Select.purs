module Store.Select where

import Prelude

import Unsafe.Reference (unsafeRefEq)

-- Selectorは、任意の型a同士の比較関数と、storeから型aを選択する関数からなる
newtype Selector store a = Selector { eq :: a -> a -> Boolean, select :: store -> a }

-- Selectorを生成する
select :: forall store a. (a -> a -> Boolean) -> (store -> a) -> Selector store a
select equalFn = Selector <<< { eq: equalFn, select: _ }
-- イータ変換や省略を用いない場合以下のようになる
-- select eq store = (Selector <<< (\s -> {eq, select: s})) store

-- Selectorを生成する
-- 比較には型aのeq関数が使用される
selectEq :: forall store a. Eq a => (store -> a) -> Selector store a
selectEq = Selector <<< { eq, select: _ }

-- Storeそのものを返すようなSelectorを生成する
selectAll :: forall store. Selector store store
selectAll = Selector { eq: unsafeRefEq, select: identity }