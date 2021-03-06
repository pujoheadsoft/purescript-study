module Study.Control.Monad.Freer.FreerDerivation where

import Data.Exists (Exists)

---------------------------------
--
-- Freer モナドを導出するステップ
--
---------------------------------

-- Freeの定義
data Free f a = 
  Pure1 a 
  | Free1 (f (Free f a))

-- Coyonedaの定義
-- forall b. Coyoneda (b -> a) (f b) のように存在量化ができないのでこうする
data CoyonedaF f a b = CoyonedaF (b -> a) (f b)
data Coyoneda f a = Coyoneda (Exists (CoyonedaF f a))

{-
  ステップ1
  fをCoyonedaで包む。Coyonedaの力でFunctorの定義が不要になる。
-}
type Freer1 f a = Free (Coyoneda f) a

{-
  ステップ2
  Freeの定義を展開。Coyonedaを代入した形となる。
-}
data Freer2 f a =
  Pure2 a
  | Impure2 (Coyoneda f (Freer2 f a))

{-
  ステップ3
    Impure2 (Coyoneda f (Freer2 f a))
  のCoyonedaの部分を展開する。

  Coyonedaの定義は以下であった(存在量化できないが、できるとして書く)
    Coyoneda f a = forall b. Coyoneda (b -> a) (f b)

  a の部分は (Freer2 f a) にあたるので、代入すると
    forall b. Coyoneda (b -> (Freer2 f a)) (f b)
  となる。

  置き換えにあたってCoyonedaという構築子は不要なので消して元の定義に代入すると
  forall b. Impure2 (b -> (Freer2 f a)) (f b)
  となる。

  更に、(b -> (Freer2 f a)) (f b) の順序を入れ替えて(もはや元の順序のままである必要がないので)
    (f b) (b -> (Freer2 f a)) 
  とすると、何か bind の定義
    (m a) -> (a -> m b) -> m b
  に似ていないだろうか？
  fを「何か」を包むものと考えると(m a)と同じで、次の関数は包みを剥がして引数に渡す関数という意味で同じだ。
  ただし、bindの場合は、(m b)を返すが、こっちはそれをFreer2で更に包んで返しているという違いがある。
  
  これをコードにする場合、存在量化ができないので以下のような型を別に定義して使う
    data FreerBindF f a b = FreerBindF (f b) (b -> (Freer f a))
  ついでに構築子の名前もわかりやすくBindにしてみる
-}

-- これが完成形
data FreerBindF3 f a b = FreerBindF3 (f b) (b -> (Freer3 f a)) 
data Freer3 f a =
  Pure3 a
  | Bind3 (Exists (FreerBindF3 f a))
