module Data.MyCoyoneda where

import Prelude

import Data.Exists (Exists, mkExists, runExists)

data CoyonedaF f a b = CoyonedaF (b -> a) (f b)

--
-- Haskellだと以下のように書けるが、PureScriptの場合ここでforallが使えないため、かわりにExistsを使う。
-- data Coyoneda f a = forall b. Coyoneda (b -> a) (f b)
-- CoyonedaFみたいなのを用意しているのもExistsのため。
--
data Coyoneda f a = Coyoneda (Exists (CoyonedaF f a))

instance coyonedaFunctor :: Functor (Coyoneda f) where
  map f (Coyoneda c) = runExists (\(CoyonedaF g a) -> coyoneda (g >>> f) a) c

-- Existsが絡んで生成が面倒だから生成関数を用意
coyoneda :: forall f a b. (a -> b) -> f a -> Coyoneda f b
coyoneda f a = Coyoneda $ mkExists $ CoyonedaF f a